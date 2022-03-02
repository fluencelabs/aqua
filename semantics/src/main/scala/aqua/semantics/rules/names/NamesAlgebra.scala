package aqua.semantics.rules.names

import aqua.parser.lexer.{LiteralToken, Name, Token, ValueToken}
import aqua.types.{ArrowType, Type}
import cats.InjectK

trait NamesAlgebra[S[_], Alg[_]] {

  def read(name: Name[S], mustBeDefined: Boolean = true): Alg[Option[Type]]

  // TODO can be implemented via read?
  def constantDefined(name: Name[S]): Alg[Option[Type]]

  def readArrow(name: Name[S]): Alg[Option[ArrowType]]

  def define(name: Name[S], `type`: Type): Alg[Boolean]

  def defineConstant(name: Name[S], `type`: Type): Alg[Boolean]

  def defineArrow(name: Name[S], gen: ArrowType, isRoot: Boolean): Alg[Boolean]

  def beginScope(token: Token[S]): Alg[Unit]

  def streamsDefinedWithinScope(): Alg[Set[String]]

  def endScope(): Alg[Unit]
}
