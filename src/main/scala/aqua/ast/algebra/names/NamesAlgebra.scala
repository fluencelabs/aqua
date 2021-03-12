package aqua.ast.algebra.names

import aqua.ast.algebra.types.{ArrowType, Type}
import aqua.parser.lexer.{Name, Token}
import cats.InjectK
import cats.free.Free

class NamesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp[F, *], Alg]) {

  def read(name: Name[F]): Free[Alg, Type] =
    Free.liftInject[Alg](ReadName(name))

  def readArrow(name: Name[F]): Free[Alg, Option[ArrowType]] =
    Free.liftInject[Alg](ReadArrow(name))

  def define(name: Name[F], `type`: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineName(name, `type`))

  def erase(name: Name[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](EraseName(name))

  def beginScope(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope(token))

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope[F]())

}

object NamesAlgebra {

  implicit def namesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp[F, *], Alg]): NamesAlgebra[F, Alg] =
    new NamesAlgebra[F, Alg]()
}
