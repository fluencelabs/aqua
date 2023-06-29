package aqua.semantics

import aqua.parser.head.{HeadExpr, HeaderExpr, ImportExpr, ImportFromExpr}
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.{Ast, Expr}
import aqua.raw.ops.{FuncOp, SeqGroupTag}
import aqua.raw.{Raw, RawContext, RawPart}
import aqua.semantics.header.Picker
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState}
import aqua.semantics.rules.definitions.{
  DefinitionsAlgebra,
  DefinitionsInterpreter,
  DefinitionsState
}
import aqua.semantics.rules.locations.{DummyLocationsInterpreter, LocationsAlgebra, LocationsState}
import aqua.semantics.rules.names.{NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypesAlgebra, TypesInterpreter, TypesState}
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.errors.ReportErrors
import aqua.semantics.rules.errors.ErrorsAlgebra
import aqua.raw.ops.*

import cats.arrow.FunctionK
import cats.data.*
import cats.Reducible
import cats.data.Chain.*
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Monoid
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.reducible.*
import cats.syntax.traverse.*
import cats.free.CofreeInstances
import cats.syntax.semigroup.*
import cats.{Eval, Monad, Semigroup}
import monocle.Lens
import monocle.macros.GenLens
import scribe.{log, Logging}
import cats.free.Cofree

trait Semantics[S[_], C] {

  def process(
    ast: Ast[S],
    init: C
  ): ValidatedNec[SemanticError[S], C]
}

class RawSemantics[S[_]](implicit p: Picker[RawContext]) extends Semantics[S, RawContext] {

  def process(
    ast: Ast[S],
    init: RawContext
  ): ValidatedNec[SemanticError[S], RawContext] = {

    implicit val locationsInterpreter: DummyLocationsInterpreter[S, CompilerState[S]] =
      new DummyLocationsInterpreter[S, CompilerState[S]]()

    RawSemantics
      .interpret(ast, CompilerState.init(init), init)
      .map { case (state, ctx) =>
        NonEmptyChain
          .fromChain(state.errors)
          .toInvalid(ctx)
      }
      // TODO: return as Eval
      .value
  }
}

object RawSemantics extends Logging {

  private final case class RawTagWithToken[S[_]](
    tree: RawTag.Tree,
    token: Token[S]
  ) {
    lazy val tag: RawTag = tree.head

    private def modifyTree(f: RawTag.Tree => RawTag.Tree): RawTagWithToken[S] =
      copy(tree = f(tree))

    def append(next: RawTagWithToken[S]): RawTagWithToken[S] = modifyTree(tree =>
      tree.copy(
        tail = (
          tree.tail,
          next.tree.tail.map(SeqTag.wrap)
        ).mapN(_ :+ _)
      )
    )

    def wrapIn(tag: GroupTag): RawTagWithToken[S] = modifyTree(tree => tag.wrap(tree))

    def toRaw: RawWithToken[S] = RawWithToken(FuncOp(tree), token)
  }

  private def elseWithoutIf[S[_], G[_]](
    token: Token[S]
  )(using E: ErrorsAlgebra[S, G]): G[Unit] =
    E.report(token, "Unexpected `else` without `if`" :: Nil)

  private def catchWithoutTry[S[_], G[_]](
    token: Token[S]
  )(using E: ErrorsAlgebra[S, G]): G[Unit] =
    E.report(token, "Unexpected `catch` without `try`" :: Nil)

  private def otherwiseWithoutPrev[S[_], G[_]](
    token: Token[S]
  )(using E: ErrorsAlgebra[S, G]): G[Unit] =
    E.report(token, "Unexpected `otherwise` without previous instruction" :: Nil)

  private def parWithoutPrev[S[_], G[_]](
    token: Token[S]
  )(using E: ErrorsAlgebra[S, G]): G[Unit] =
    E.report(token, "Unexpected `par` without previous instruction" :: Nil)

  private def rawTagCombine[S[_], G[_]: Monad](
    prev: RawTagWithToken[S],
    next: RawTagWithToken[S]
  )(using E: ErrorsAlgebra[S, G]): G[Option[RawTagWithToken[S]]] =
    (prev.tag, next.tag) match {
      case (_: IfTag, IfTag.Else) =>
        prev.append(next).some.pure
      case (_, IfTag.Else) | (IfTag.Else, _) =>
        val token = prev.tag match {
          case IfTag.Else => prev.token
          case _ => next.token
        }

        elseWithoutIf(token).as(none)

      case (TryTag, TryTag.Catch) =>
        prev.append(next).some.pure
      case (_, TryTag.Catch) | (TryTag.Catch, _) =>
        val token = prev.tag match {
          case TryTag.Catch => prev.token
          case _ => next.token
        }

        catchWithoutTry(token).as(none)

      case (TryTag.Otherwise, _) =>
        otherwiseWithoutPrev(prev.token).as(none)
      case (TryTag, TryTag.Otherwise) =>
        prev.append(next).some.pure
      case (_, TryTag.Otherwise) =>
        prev
          .wrapIn(TryTag)
          .append(next)
          .some
          .pure

      case (ParTag.Par, _) =>
        parWithoutPrev(prev.token).as(none)
      case (ParTag, ParTag.Par) =>
        prev.append(next).some.pure
      case (_, ParTag.Par) =>
        prev
          .wrapIn(ParTag)
          .append(next)
          .some
          .pure

      case _ => none.pure
    }

  private def rawTagSingleCheck[S[_], G[_]: Monad](
    single: RawTagWithToken[S]
  )(using E: ErrorsAlgebra[S, G]): G[Option[RawTagWithToken[S]]] =
    single.tag match {
      case IfTag.Else => elseWithoutIf(single.token).as(none)
      case TryTag.Catch => catchWithoutTry(single.token).as(none)
      case TryTag.Otherwise => otherwiseWithoutPrev(single.token).as(none)
      case _ => single.some.pure
    }

  private final case class RawWithToken[S[_]](
    raw: Raw,
    token: Token[S]
  ) {

    def toTag: Option[RawTagWithToken[S]] =
      raw match {
        case FuncOp(tree) => RawTagWithToken(tree, token).some
        case _ => none
      }

  }

  private final case class InnersFoldState[S[_]](
    last: Option[RawWithToken[S]] = None,
    acc: Chain[Raw] = Chain.empty
  ) {

    def step[G[_]: Monad](
      next: RawWithToken[S]
    )(using ErrorsAlgebra[S, G]): G[InnersFoldState[S]] =
      last.fold(copy(last = next.some).pure)(prev =>
        (prev.toTag, next.toTag)
          .traverseN(rawTagCombine)
          .map(
            _.flatten.fold(
              copy(
                last = next.some,
                acc = prev.raw +: acc
              )
            )(combined =>
              copy(
                last = combined.toRaw.some
              )
            )
          )
      )

    def result[G[_]: Monad](using
      ErrorsAlgebra[S, G]
    ): G[Option[Raw]] =
      if (acc.isEmpty)
        last.flatTraverse(single =>
          single.toTag.fold(single.raw.some.pure)(singleTag =>
            for {
              checked <- rawTagSingleCheck(singleTag)
              maybeRaw = checked.map(_.toRaw.raw)
            } yield maybeRaw
          )
        )
      else
        last
          .fold(acc)(_.raw +: acc)
          .reverse
          .reduceLeftOption(_ |+| _)
          .pure
  }

  private def folder[S[_], G[_]: Monad](implicit
    A: AbilitiesAlgebra[S, G],
    N: NamesAlgebra[S, G],
    T: TypesAlgebra[S, G],
    D: DefinitionsAlgebra[S, G],
    L: LocationsAlgebra[S, G],
    E: ErrorsAlgebra[S, G]
  ): (Expr[S], Chain[G[RawWithToken[S]]]) => Eval[G[RawWithToken[S]]] = (expr, inners) =>
    Eval later ExprSem
      .getProg[S, G](expr)
      .apply(for {
        children <- inners.sequence
        resultState <- children
          .traverse(raw => StateT.modifyF((state: InnersFoldState[S]) => state.step(raw)))
          .runS(InnersFoldState())
        result <- resultState.result
      } yield result.getOrElse(Raw.empty("AST is empty")))
      .map(raw => RawWithToken(raw, expr.token))

  type Interpreter[S[_], A] = State[CompilerState[S], A]

  def transpile[S[_]](
    ast: Ast[S]
  )(implicit locations: LocationsAlgebra[S, Interpreter[S, *]]): Interpreter[S, Raw] = {
    import monocle.syntax.all.*

    implicit val re: ReportErrors[S, CompilerState[S]] = new ReportErrors[S, CompilerState[S]] {
      override def apply(
        st: CompilerState[S],
        token: Token[S],
        hints: List[String]
      ): CompilerState[S] =
        st.focus(_.errors).modify(_.append(RulesViolated(token, hints)))
    }

    implicit val ns: Lens[CompilerState[S], NamesState[S]] = GenLens[CompilerState[S]](_.names)

    implicit val as: Lens[CompilerState[S], AbilitiesState[S]] =
      GenLens[CompilerState[S]](_.abilities)

    implicit val ts: Lens[CompilerState[S], TypesState[S]] = GenLens[CompilerState[S]](_.types)

    implicit val ds: Lens[CompilerState[S], DefinitionsState[S]] =
      GenLens[CompilerState[S]](_.definitions)

    implicit val typesInterpreter: TypesInterpreter[S, CompilerState[S]] =
      new TypesInterpreter[S, CompilerState[S]]
    implicit val abilitiesInterpreter: AbilitiesInterpreter[S, CompilerState[S]] =
      new AbilitiesInterpreter[S, CompilerState[S]]
    implicit val namesInterpreter: NamesInterpreter[S, CompilerState[S]] =
      new NamesInterpreter[S, CompilerState[S]]
    implicit val definitionsInterpreter: DefinitionsInterpreter[S, CompilerState[S]] =
      new DefinitionsInterpreter[S, CompilerState[S]]

    ast
      .cata(folder[S, Interpreter[S, *]])
      .value
      .map(_.raw)
  }

  private def astToState[S[_]](ast: Ast[S])(implicit
    locations: LocationsAlgebra[S, Interpreter[S, *]]
  ): Interpreter[S, Raw] =
    transpile[S](ast)

  // If there are any errors, they're inside CompilerState[S]
  def interpret[S[_]](
    ast: Ast[S],
    initState: CompilerState[S],
    init: RawContext
  )(implicit
    locations: LocationsAlgebra[S, Interpreter[S, *]]
  ): Eval[(CompilerState[S], RawContext)] =
    astToState[S](ast)
      .run(initState)
      .map {
        case (state, _: Raw.Empty) =>
          // No `parts`, but has `init`
          (
            state,
            RawContext.blank.copy(
              init = Some(init.copy(module = init.module.map(_ + "|init")))
                .filter(_ != RawContext.blank)
            )
          )

        case (state, part: (RawPart | RawPart.Parts)) =>
          state -> RawPart
            .contextPart(part)
            .parts
            .foldLeft(
              RawContext.blank.copy(
                init = Some(init.copy(module = init.module.map(_ + "|init")))
                  .filter(_ != RawContext.blank)
              )
            ) { case (ctx, p) =>
              ctx.copy(parts = ctx.parts :+ (ctx -> p))
            }

        case (state: CompilerState[S], m) =>
          logger.error("Got unexpected " + m)
          state.copy(errors = state.errors :+ WrongAST(ast)) -> RawContext.blank.copy(
            init = Some(init.copy(module = init.module.map(_ + "|init")))
              .filter(_ != RawContext.blank)
          )
      }
}
