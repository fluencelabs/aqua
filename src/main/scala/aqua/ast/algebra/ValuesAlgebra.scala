package aqua.ast.algebra

import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.types.{ArrowType, LiteralType, Type, TypesAlgebra}
import aqua.parser.lexer.{Literal, Token, Value, VarLambda}
import cats.free.Free
import cats.syntax.apply._

class ValuesAlgebra[F[_], Alg[_]](implicit N: NamesAlgebra[F, Alg], T: TypesAlgebra[F, Alg]) {

  def ensureIsString(v: Value[F]): Free[Alg, Boolean] =
    v match {
      case l: Literal[F] =>
        T.ensureTypeMatches(l, LiteralType.string, l.ts)
      case v @ VarLambda(n, lambda) =>
        N.read(n).flatMap {
          case Some(t) =>
            T.resolveLambda(t, lambda).flatMap {
              case Some(vt) => T.ensureTypeMatches(v.lambda.lastOption.getOrElse(n), LiteralType.string, vt)
              case None => Free.pure(false)
            }
          case None =>
            Free.pure(false)
        }
    }

  def checkArguments(arr: ArrowType, args: List[Value[F]]): Free[Alg, Boolean] =
    args
      .map[Free[Alg, Option[(Token[F], Type)]]] {
        case l: Literal[F] => Free.pure(Some(l -> l.ts))
        case VarLambda(n, ops) =>
          N.read(n).flatMap {
            case Some(t) => T.resolveLambda(t, ops).map(_.map(ops.lastOption.getOrElse(n) -> _))
            case None => Free.pure(None)
          }
      }
      // TODO check that number of arguments matches!
      .zip(arr.args)
      .foldLeft(
        Free.pure[Alg, Boolean](true)
      ) {
        case (f, (ft, t)) =>
          (
            f,
            ft.flatMap {
              case None => Free.pure(false)
              case Some((tkn, valType)) =>
                T.ensureTypeMatches(tkn, t, valType)
            }
          ).mapN(_ && _)
      }

}

object ValuesAlgebra {

  implicit def deriveValuesAlgebra[F[_], Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): ValuesAlgebra[F, Alg] =
    new ValuesAlgebra[F, Alg]()
}
