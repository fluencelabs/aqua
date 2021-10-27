package aqua.semantics.rules.names

import aqua.parser.lexer.{Literal, Name, Token, Value}
import aqua.types.{ArrowType, Type}
import cats.InjectK
import cats.free.Free

trait NamesAlgebra[F[_], Alg[_]] {

  def read(name: Name[F], mustBeDefined: Boolean = true): Alg[Option[Type]]

  // TODO can be implemented via read?
  def constantDefined(name: Name[F]): Alg[Option[Type]]

  def readArrow(name: Name[F]): Alg[Option[ArrowType]]

  def define(name: Name[F], `type`: Type): Alg[Boolean]

  def defineConstant(name: Name[F], `type`: Type): Alg[Boolean]

  def defineArrow(name: Name[F], gen: ArrowType, isRoot: Boolean): Alg[Boolean]

  def beginScope(token: Token[F]): Alg[Unit]

  def endScope(): Alg[Unit]
}
