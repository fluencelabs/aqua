package aqua.semantics.rules

import aqua.model._
import aqua.parser.lexer._
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, LiteralType, Type}
import cats.data.Chain
import cats.free.Free
import cats.syntax.apply._

class ValuesAlgebra[F[_], Alg[_]](implicit N: NamesAlgebra[F, Alg], T: TypesAlgebra[F, Alg]) {

  def ensureIsString(v: Value[F]): Free[Alg, Boolean] =
    ensureTypeMatches(v, LiteralType.string)

  def ensureTypeMatches(v: Value[F], expected: Type): Free[Alg, Boolean] =
    resolveType(v).flatMap {
      case Some(vt) =>
        T.ensureTypeMatches(
          v match {
            case l: Literal[F] => l
            case VarLambda(n, lambda) => lambda.lastOption.getOrElse(n)
          },
          expected,
          vt
        )
      case None => Free.pure(false)
    }

  def resolveType(v: Value[F]): Free[Alg, Option[Type]] =
    v match {
      case l: Literal[F] =>
        Free pure [Alg, Option[Type]] Some(l.ts)
      case VarLambda(n, lambda) =>
        N.read(n).flatMap {
          case Some(t) =>
            T.resolveLambda(t, lambda)
          case None =>
            Free.pure(None)
        }
    }

  def checkArguments(token: Token[F], arr: ArrowType, args: List[Value[F]]): Free[Alg, Boolean] = {
    T.checkArgumentsNumber(token, arr.args.length, args.length).flatMap {
      case false => Free.pure[Alg, Boolean](false)
      case true =>
        args
          .map[Free[Alg, Option[(Token[F], Type)]]] {
            case l: Literal[F] => Free.pure(Some(l -> l.ts))
            case VarLambda(n, ops) =>
              N.read(n).flatMap {
                case Some(t) => T.resolveLambda(t, ops).map(_.map(ops.lastOption.getOrElse(n) -> _))
                case None => Free.pure(None)
              }
          }
          .zip(arr.args)
          .foldLeft(
            Free.pure[Alg, Boolean](true)
          ) { case (f, (ft, t)) =>
            (
              f,
              ft.flatMap {
                case None =>
                  Free.pure(false)
                case Some((tkn, valType)) =>
                  T.ensureTypeMatches(tkn, t, valType)
              }
            ).mapN(_ && _)
          }
    }
  }

}

object ValuesAlgebra {

  private def opsToModel[F[_]](ops: List[LambdaOp[F]]): Chain[LambdaModel] =
    ops match {
      case Nil => Chain.empty
      case (_: IntoArray[F]) :: tail => opsToModel(tail).prepend(IntoArrayModel)
      case (f: IntoField[F]) :: tail => opsToModel(tail).prepend(IntoFieldModel(f.value))
    }

  def valueToModel[F[_]](v: Value[F]): ValueModel =
    v match {
      case l: Literal[F] => LiteralModel(l.value)
      case VarLambda(name, ops) => VarModel(name.value, opsToModel(ops))
    }

  implicit def deriveValuesAlgebra[F[_], Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): ValuesAlgebra[F, Alg] =
    new ValuesAlgebra[F, Alg]()
}
