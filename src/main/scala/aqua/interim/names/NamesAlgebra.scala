package aqua.interim.names

import aqua.interim.types.{ArrowType, Type}
import aqua.parser.lexer.{Name, Token}
import cats.InjectK
import cats.free.Free

class NamesAlgebra[Alg[_]](implicit V: InjectK[NameOp, Alg]) {

  def read[F[_]](name: Name[F]): Free[Alg, Type] =
    Free.liftInject[Alg](ReadName(name))

  def readArrow[F[_]](name: Name[F]): Free[Alg, ArrowType] =
    Free.liftInject[Alg](ReadArrow(name))

  def define[F[_]](name: Name[F], `type`: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineName(name, `type`))

  def erase[F[_]](name: Name[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](EraseName(name))

  def beginScope[F[_]](token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope(token))

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope())

}

object NamesAlgebra {
  implicit def namesAlgebra[Alg[_]](implicit V: InjectK[NameOp, Alg]): NamesAlgebra[Alg] = new NamesAlgebra[Alg]()
}
