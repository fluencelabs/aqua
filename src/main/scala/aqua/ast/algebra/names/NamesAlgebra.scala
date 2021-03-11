package aqua.ast.algebra.names

import aqua.ast.algebra.types.{ArrowType, Type}
import aqua.parser.lexer.{Name, Token}
import cats.InjectK
import cats.free.Free

class NamesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp.Aux[F, *], Alg]) {

  def read(name: Name[F]): Free[Alg, Type] =
    Free.liftInject[Alg](ReadName(name): NameOp.Aux[F, Type])

  def readArrow(name: Name[F]): Free[Alg, ArrowType] =
    Free.liftInject[Alg](ReadArrow(name): NameOp.Aux[F, ArrowType])

  def define(name: Name[F], `type`: Type): Free[Alg, Unit] =
    Free.liftInject[Alg](DefineName(name, `type`): NameOp.Aux[F, Unit])

  def erase(name: Name[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](EraseName(name): NameOp.Aux[F, Unit])

  def beginScope(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope(token): NameOp.Aux[F, Unit])

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope(): NameOp.Aux[F, Unit])

}

object NamesAlgebra {

  implicit def namesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp.Aux[F, *], Alg]): NamesAlgebra[F, Alg] =
    new NamesAlgebra[F, Alg]()
}
