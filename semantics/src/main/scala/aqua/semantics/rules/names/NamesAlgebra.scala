package aqua.semantics.rules.names

import aqua.parser.lexer.{Literal, Name, Token, Value}
import aqua.types.{ArrowType, Type}
import cats.InjectK
import cats.free.Free

class NamesAlgebra[F[_], Alg[_]](implicit V: InjectK[NameOp[F, *], Alg]) {

  def read(name: Name[F], mustBeDefined: Boolean = true): Free[Alg, Option[Type]] =
    Free.liftInject[Alg](ReadName(name, mustBeDefined))

  // TODO can be implemented via read?
  def constantDefined(name: Name[F]): Free[Alg, Option[Type]] =
    Free.liftInject[Alg](ConstantDefined(name))

  def readArrow(name: Name[F]): Free[Alg, Option[ArrowType]] =
    Free.liftInject[Alg](ReadArrow(name))

  def define(name: Name[F], `type`: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineName(name, `type`))

  def defineOpaque(name: Name[F], `type`: Type): Free[Alg, Name[F]] =
    Free.liftInject[Alg](DefineOpaqueName(name, `type`))

  def defineConstant(name: Name[F], `type`: Type): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineConstant(name, `type`))

  def defineArrow(name: Name[F], gen: ArrowType, isRoot: Boolean): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineArrow(name, gen, isRoot))

  def beginScope(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope(token))

  def endScope(): Free[Alg, Unit] =
    Free.liftInject[Alg](EndScope[F]())

}

object NamesAlgebra {

  implicit def namesAlgebra[F[_], Alg[_]](implicit
    V: InjectK[NameOp[F, *], Alg]
  ): NamesAlgebra[F, Alg] =
    new NamesAlgebra[F, Alg]()
}
