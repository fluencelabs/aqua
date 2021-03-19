package aqua.semantics.algebra.names

import aqua.semantics.algebra.types.{ArrowType, Type}
import aqua.generator.ArrowGen
import aqua.parser.lexer.{Name, Token}
import cats.InjectK
import cats.free.Free

class NamesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp[F, *], Alg]) {

  def read(name: Name[F]): Free[Alg, Option[Type]] =
    Free.liftInject[Alg](ReadName(name))

  def readArrow(name: Name[F]): Free[Alg, Option[ArrowGen]] =
    Free.liftInject[Alg](ReadArrow(name))

  def define(name: Name[F], `type`: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineName(name, `type`))

  def defineArrow(name: Name[F], gen: ArrowGen, isRoot: Boolean): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineArrow(name, gen, isRoot))

  def beginScope(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope(token))

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope[F]())

}

object NamesAlgebra {

  implicit def namesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp[F, *], Alg]): NamesAlgebra[F, Alg] =
    new NamesAlgebra[F, Alg]()
}
