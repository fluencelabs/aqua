package aqua.semantics.rules.names

import aqua.parser.lexer.{LiteralToken, Name, Token, ValueToken}
import aqua.types.{ArrowType, StreamType, Type}
import cats.InjectK

trait NamesAlgebra[S[_], Alg[_]] {

  def read(name: Name[S], mustBeDefined: Boolean = true): Alg[Option[Type]]

  def readArrow(name: Name[S]): Alg[Option[ArrowType]]

  /**
   *
   * @param canOverride internal flag that is used for overriding services by serviceId
   */
  def define(name: Name[S], `type`: Type, canOverride: Boolean = false): Alg[Boolean]

  def derive(name: Name[S], `type`: Type, derivedFrom: Set[String]): Alg[Boolean]

  def getDerivedFrom(fromNames: List[Set[String]]): Alg[List[Set[String]]]

  def defineRootValue(name: Name[S], `type`: Type): Alg[Boolean]

  def defineArrow(name: Name[S], gen: ArrowType, isRoot: Boolean): Alg[Boolean]

  def streamsDefinedWithinScope(): Alg[Map[String, StreamType]]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]
}
