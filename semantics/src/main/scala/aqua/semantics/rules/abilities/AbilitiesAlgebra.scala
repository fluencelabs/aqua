package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Name, NamedTypeToken, Token, ValueToken}
import aqua.raw.value.ValueRaw
import aqua.types.{ArrowType, ServiceType}

import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilitiesAlgebra[S[_], Alg[_]] {

  def defineService(
    name: NamedTypeToken[S],
    arrowDefs: NonEmptyMap[String, Name[S]],
    defaultId: Option[ValueRaw]
  ): Alg[Boolean]

  def isDefinedAbility(name: NamedTypeToken[S]): Alg[Boolean]

  def getArrow(name: NamedTypeToken[S], arrow: Name[S]): Alg[Option[ArrowType]]

  def renameService(name: NamedTypeToken[S]): Alg[Option[String]]

  def getServiceRename(name: NamedTypeToken[S]): Alg[Option[String]]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]

}
