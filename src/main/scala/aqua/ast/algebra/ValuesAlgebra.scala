package aqua.ast.algebra

import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.types.{ArrowType, LiteralType, Type, TypesAlgebra}
import aqua.parser.lexer.{Literal, Token, Value, VarLambda}
import cats.free.Free
import cats.syntax.flatMap._

class ValuesAlgebra[F[_], Alg[_]](implicit N: NamesAlgebra[F, Alg], T: TypesAlgebra[F, Alg]) {

  def ensureIsString(v: Value[F]): Free[Alg, Unit] =
    v match {
      case l: Literal[F] =>
        T.ensureTypeMatches(l, LiteralType.string, l.ts)
      case v @ VarLambda(n, lambda) =>
        for {
          t <- N.read(n)
          vt <- T.resolveLambda(t, lambda)
          _ <- T.ensureTypeMatches(v.lambda.lastOption.getOrElse(n), LiteralType.string, vt)
        } yield ()
    }

  def checkArguments(arr: ArrowType, args: List[Value[F]]): Free[Alg, Unit] =
    args
      .map[Free[Alg, (Token[F], Type)]] {
        case l: Literal[F] => Free.pure(l -> l.ts)
        case VarLambda(n, ops) =>
          N.read(n)
            .flatMap(T.resolveLambda(_, ops))
            .map(ops.lastOption.getOrElse(n) -> _)
      }
      // TODO check that number of arguments matches!
      .zip(arr.args)
      .foldLeft(
        Free.pure[Alg, Unit](())
      ) {
        case (f, (ft, t)) =>
          f >> ft.flatMap(tknAndValType => T.ensureTypeMatches(tknAndValType._1, t, tknAndValType._2))
      }

}

object ValuesAlgebra {

  implicit def deriveValuesAlgebra[F[_], Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): ValuesAlgebra[F, Alg] =
    new ValuesAlgebra[F, Alg]()
}
