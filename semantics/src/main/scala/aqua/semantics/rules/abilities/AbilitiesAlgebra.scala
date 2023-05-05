package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Ability, NamedTypeToken, Name, Token, ValueToken}
import aqua.raw.value.ValueRaw
import aqua.types.ArrowType
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilitiesAlgebra[S[_], Alg[_]] {

  def defineService(
    name: NamedTypeToken[S],
    arrows: NonEmptyMap[String, (Name[S], ArrowType)],
    defaultId: Option[ValueRaw]
  ): Alg[Boolean]

  def getArrow(name: NamedTypeToken[S], arrow: Name[S]): Alg[Option[ArrowType]]

  def setServiceId(name: NamedTypeToken[S], id: ValueToken[S], vm: ValueRaw): Alg[Boolean]

  def getServiceId(name: NamedTypeToken[S]): Alg[Either[Boolean, ValueRaw]]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]

}
