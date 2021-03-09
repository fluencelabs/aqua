package aqua.interim

import aqua.interim.names.NamesAlgebra
import aqua.interim.types.{LiteralType, TypesAlgebra}
import aqua.parser.lexer.{Literal, Value, Var, VarLambda}
import cats.free.Free

class VarsAlgebra[Alg[_]](implicit N: NamesAlgebra[Alg], T: TypesAlgebra[Alg]) {

  def ensureIsString[F[_]](v: Value[F]): Free[Alg, Unit] =
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

}
