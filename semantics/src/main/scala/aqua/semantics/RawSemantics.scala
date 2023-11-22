package aqua.semantics

import aqua.errors.Errors.internalError
import aqua.raw.ops.*
import aqua.semantics.rules.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState}
import aqua.semantics.rules.definitions.{DefinitionsAlgebra, DefinitionsInterpreter}
import aqua.semantics.rules.locations.{DummyLocationsInterpreter, LocationsAlgebra}
import aqua.semantics.rules.names.{NamesAlgebra, NamesInterpreter}
import aqua.semantics.rules.mangler.{ManglerAlgebra, ManglerInterpreter}
import aqua.semantics.rules.types.{TypesAlgebra, TypesInterpreter}
import aqua.semantics.rules.report.{ReportAlgebra, ReportInterpreter}
import aqua.semantics.header.Picker
import aqua.semantics.header.Picker.*
import aqua.raw.{Raw, RawContext, RawPart}
import aqua.parser.{Ast, Expr}
import aqua.parser.lexer.{LiteralToken, Token}

import cats.{Eval, Monad}
import cats.data.{Chain, EitherT, NonEmptyChain, State, StateT, ValidatedNec, Writer}
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.reducible.*
import cats.syntax.traverse.*
import cats.syntax.semigroup.*
import scribe.Logging

class RawSemantics[S[_]](using
  Picker[RawContext]
) extends Semantics[S, RawContext] {

  override def process(
    ast: Ast[S],
    init: RawContext
  ): ProcessResult = {

    given LocationsAlgebra[S, State[CompilerState[S], *]] =
      new DummyLocationsInterpreter[S, CompilerState[S]]()

    RawSemantics
      .interpret(ast, CompilerState.init(init), init)
      .map { case (state, ctx) =>
        EitherT(
          Writer
            .tell(state.warnings)
            .as(
              NonEmptyChain
                .fromChain(state.errors)
                .toLeft(ctx)
            )
        )
      }
      // TODO: return as Eval
      .value
  }
}

object RawSemantics extends Logging {

  /**
   * [[RawTag.Tree]] with [[Token]] used for error reporting
   */
  private final case class RawTagWithToken[S[_]](
    tree: RawTag.Tree,
    token: Token[S]
  ) {
    lazy val tag: RawTag = tree.head

    private def modifyTree(f: RawTag.Tree => RawTag.Tree): RawTagWithToken[S] =
      copy(tree = f(tree))

    /**
     * Wrap tail of @param next in [[SeqTag]]
     * and append it to current tree tail
     */
    def append(next: RawTagWithToken[S]): RawTagWithToken[S] = modifyTree(tree =>
      tree.copy(
        tail = (
          tree.tail,
          // SeqTag.wrap will return single node as is
          next.tree.tail.map(SeqTag.wrap)
        ).mapN(_ :+ _)
      )
    )

    def wrapIn(tag: GroupTag): RawTagWithToken[S] = modifyTree(tree => tag.wrap(tree))

    def toRaw: RawWithToken[S] = RawWithToken(FuncOp(tree), token)
  }

  private def elseWithoutIf[S[_], G[_]](
    token: Token[S]
  )(using report: ReportAlgebra[S, G]): G[Unit] =
    report.error(token, "Unexpected `else` without `if`" :: Nil)

  private def catchWithoutTry[S[_], G[_]](
    token: Token[S]
  )(using report: ReportAlgebra[S, G]): G[Unit] =
    report.error(token, "Unexpected `catch` without `try`" :: Nil)

  private def otherwiseWithoutPrev[S[_], G[_]](
    token: Token[S]
  )(using report: ReportAlgebra[S, G]): G[Unit] =
    report.error(token, "Unexpected `otherwise` without previous instruction" :: Nil)

  private def parWithoutPrev[S[_], G[_]](
    token: Token[S]
  )(using report: ReportAlgebra[S, G]): G[Unit] =
    report.error(token, "Unexpected `par` without previous instruction" :: Nil)

  /**
   * Optionally combine two [[RawTag.Tree]] into one.
   * Used to combine `if` and `else`,
   * `try` and `catch` (`otherwise`);
   * to create [[ParTag]] from `par`,
   * [[TryTag]] from `otherwise`
   *
   * @param prev Previous tag
   * @param next Next tag
   * @param E Algebra for error reporting
   * @return    [[Some]] with result of combination
   *            [[None]] if tags should not be combined
   *                        or error occuried
   */
  private def rawTagCombine[S[_], G[_]: Monad](
    prev: RawTagWithToken[S],
    next: RawTagWithToken[S]
  )(using E: ReportAlgebra[S, G]): G[Option[RawTagWithToken[S]]] =
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

  /**
   * Check if tag is valid to be single
   *
   * @param single tag
   * @param E Algebra for error reporting
   * @return   [[Some]] if tag is valid to be single
   *           [[None]] otherwise
   */
  private def rawTagSingleCheck[S[_], G[_]: Monad](
    single: RawTagWithToken[S]
  )(using E: ReportAlgebra[S, G]): G[Option[RawTagWithToken[S]]] =
    single.tag match {
      case IfTag.Else => elseWithoutIf(single.token).as(none)
      case TryTag.Catch => catchWithoutTry(single.token).as(none)
      case TryTag.Otherwise => otherwiseWithoutPrev(single.token).as(none)
      case ParTag.Par => parWithoutPrev(single.token).as(none)
      case _ => single.some.pure
    }

  /**
   * [[Raw]] with [[Token]] used for error reporting
   */
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

  /**
   * State for folding [[Raw]] results of children
   *
   * @param last Last seen [[Raw]] with [[Token]]
   * @param acc All previous [[Raw]]
   */
  private final case class InnersFoldState[S[_]](
    last: Option[RawWithToken[S]] = None,
    acc: Chain[Raw] = Chain.empty
  ) {

    /**
     * Process new incoming [[Raw]]
     */
    def step[G[_]: Monad](
      next: RawWithToken[S]
    )(using ReportAlgebra[S, G]): G[InnersFoldState[S]] =
      last.fold(copy(last = next.some).pure)(prev =>
        (prev.toTag, next.toTag)
          .traverseN(rawTagCombine)
          .map(
            _.flatten.fold(
              // No combination - just update last and acc
              copy(
                last = next.some,
                acc = prev.raw +: acc
              )
            )(combined =>
              // Result of combination is the new last
              copy(
                last = combined.toRaw.some
              )
            )
          )
      )

    /**
     * Produce result of folding
     */
    def result[G[_]: Monad](using
      ReportAlgebra[S, G]
    ): G[Option[Raw]] =
      if (acc.isEmpty)
        // Hack to report error if single tag in block is incorrect
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

  private def folder[S[_], G[_]: Monad](using
    A: AbilitiesAlgebra[S, G],
    N: NamesAlgebra[S, G],
    T: TypesAlgebra[S, G],
    D: DefinitionsAlgebra[S, G],
    L: LocationsAlgebra[S, G],
    E: ReportAlgebra[S, G],
    M: ManglerAlgebra[G]
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

  def transpile[S[_]](ast: Ast[S])(using
    LocationsAlgebra[S, Interpreter[S, *]]
  ): Interpreter[S, Raw] = {

    given ReportAlgebra[S, Interpreter[S, *]] =
      new ReportInterpreter[S, CompilerState[S]]
    given TypesAlgebra[S, Interpreter[S, *]] =
      new TypesInterpreter[S, CompilerState[S]]
    given ManglerAlgebra[Interpreter[S, *]] =
      new ManglerInterpreter[CompilerState[S]]
    given AbilitiesAlgebra[S, Interpreter[S, *]] =
      new AbilitiesInterpreter[S, CompilerState[S]]
    given NamesAlgebra[S, Interpreter[S, *]] =
      new NamesInterpreter[S, CompilerState[S]]
    given DefinitionsAlgebra[S, Interpreter[S, *]] =
      new DefinitionsInterpreter[S, CompilerState[S]]

    ast
      .cata(folder[S, Interpreter[S, *]])
      .value
      .map(_.raw)
  }

  private def astToState[S[_]](ast: Ast[S])(using
    locations: LocationsAlgebra[S, Interpreter[S, *]]
  ): Interpreter[S, Raw] =
    transpile[S](ast)

  // If there are any errors, they're inside CompilerState[S]
  def interpret[S[_]](
    ast: Ast[S],
    initState: CompilerState[S],
    init: RawContext
  )(using
    LocationsAlgebra[S, Interpreter[S, *]]
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

        case (_, m) =>
          internalError(
            s"Unexpected Raw ($m)"
          )
      }
}
