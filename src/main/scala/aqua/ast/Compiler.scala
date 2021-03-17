package aqua.ast

import aqua.ast.algebra.ReportError
import aqua.ast.algebra.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState, AbilityOp}
import aqua.ast.algebra.names.{NameOp, NamesAlgebra, NamesInterpreter, NamesState}
import aqua.ast.algebra.scope.{PeerIdAlgebra, PeerIdInterpreter, PeerIdOp, PeerIdState}
import aqua.ast.algebra.types.{TypeOp, TypesAlgebra, TypesInterpreter, TypesState}
import aqua.ast.expr._
import aqua.ast.gen.Gen
import aqua.parser.lexer.Token
import cats.Eval
import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{EitherK, NonEmptyList, State, ValidatedNel}
import cats.free.Free
import monocle.Lens
import monocle.macros.GenLens
import cats.syntax.apply._
import cats.syntax.semigroup._

import scala.collection.immutable.Queue

object Compiler {

  private def exprToProg[F[_], G[_]](
    expr: Expr[F]
  )(implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    P: PeerIdAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): Prog[G, Gen] =
    expr match {
      case expr: AbilityIdExpr[F] => expr.program[G]
      case expr: AliasExpr[F] => expr.program[G]
      case expr: ArrowTypeExpr[F] => expr.program[G]
      case expr: CoalgebraExpr[F] => expr.program[G]
      case expr: DataStructExpr[F] => expr.program[G]
      case expr: FieldTypeExpr[F] => expr.program[G]
      case expr: FuncExpr[F] => expr.program[G]
      case expr: OnExpr[F] => expr.program[G]
      case expr: ParExpr[F] => expr.program[G]
      case expr: ReturnExpr[F] => expr.program[G]
      case expr: ServiceExpr[F] => expr.program[G]
      case expr: RootExpr[F] => expr.program[G]
    }

  def folder[F[_], G[_]](implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    P: PeerIdAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): (Expr[F], List[Free[G, Gen]]) => Eval[Free[G, Gen]] = {
    case (expr, inners) =>
      Eval later exprToProg[F, G](expr)
        .apply(
          inners
            .reduceLeftOption[Free[G, Gen]]((a, b) => (a, b).mapN(_ |+| _))
            .getOrElse(Free.pure(Gen.noop))
        )
  }

  type Alg0[F[_], A] = EitherK[AbilityOp[F, *], NameOp[F, *], A]
  type Alg1[F[_], A] = EitherK[PeerIdOp[F, *], Alg0[F, *], A]
  type Alg[F[_], A] = EitherK[TypeOp[F, *], Alg1[F, *], A]

  def transpile[F[_]](ast: Ast[F]): Free[Alg[F, *], Gen] =
    ast.cata(folder[F, Alg[F, *]]).value

  case class CompilerState[F[_]](
    errors: Queue[(Token[F], String)] = Queue.empty[(Token[F], String)],
    names: NamesState[F] = NamesState[F](),
    abilities: AbilitiesState[F] = AbilitiesState[F](),
    peerId: PeerIdState[F] = PeerIdState[F](),
    types: TypesState[F] = TypesState[F]()
  )

  def interpret[F[_]](free: Free[Alg[F, *], Gen]): State[CompilerState[F], Gen] = {
    import monocle.macros.syntax.all._

    implicit val re: ReportError[F, CompilerState[F]] =
      (st: CompilerState[F], token: Token[F], hint: String) => st.focus(_.errors).modify(_.enqueue(token -> hint))

    implicit val ns: Lens[CompilerState[F], NamesState[F]] = GenLens[CompilerState[F]](_.names)

    val names = new NamesInterpreter[F, CompilerState[F]]()

    implicit val as: Lens[CompilerState[F], AbilitiesState[F]] = GenLens[CompilerState[F]](_.abilities)

    val abilities = new AbilitiesInterpreter[F, CompilerState[F]]()

    implicit val ps: Lens[CompilerState[F], PeerIdState[F]] = GenLens[CompilerState[F]](_.peerId)

    val peerId = new PeerIdInterpreter[F, CompilerState[F]]()

    implicit val ts: Lens[CompilerState[F], TypesState[F]] = GenLens[CompilerState[F]](_.types)

    val types = new TypesInterpreter[F, CompilerState[F]]()

    val interpreter0: FunctionK[Alg0[F, *], State[CompilerState[F], *]] = abilities or names
    val interpreter1: FunctionK[Alg1[F, *], State[CompilerState[F], *]] = peerId or interpreter0
    val interpreter: FunctionK[Alg[F, *], State[CompilerState[F], *]] = types or interpreter1

    free.foldMap[State[CompilerState[F], *]](interpreter)
  }

  def compile[F[_]](ast: Ast[F]): ValidatedNel[(Token[F], String), Gen] =
    (transpile[F] _ andThen interpret[F])(ast)
      .run(CompilerState[F]())
      .map {
        case (state, gen) =>
          NonEmptyList.fromList(state.errors.toList).fold[ValidatedNel[(Token[F], String), Gen]](Valid(gen))(Invalid(_))
      }
      .value
}
