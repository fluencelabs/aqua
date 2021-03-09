package aqua.interim.names

import aqua.interim.types.Type
import aqua.parser.lexer.{Token, Var}
import cats.InjectK
import cats.free.Free

class NamesAlgebra[Alg[_]](implicit V: InjectK[NameOp, Alg]) {

  def read[F[_]](name: Var[F]): Free[Alg, Type] =
    Free.liftInject[Alg](ReadName(name))

  def define[F[_]](name: Var[F], `type`: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineName(name, `type`))

  def erase[F[_]](name: Var[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](EraseName(name))

  def beginScope[F[_]](token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope(token))

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope())

}
