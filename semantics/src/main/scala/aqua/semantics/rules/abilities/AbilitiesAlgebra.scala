package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Ability, IntoArrow, Name, Token, ValueToken}
import aqua.raw.value.{PropertyRaw, ValueRaw}
import aqua.types.{ArrowType, Type}
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilitiesAlgebra[S[_], Alg[_]] {

  def defineArrow(arrow: Name[S], `type`: ArrowType): Alg[Boolean]

  def purgeArrows(token: Token[S]): Alg[Option[NonEmptyList[(Name[S], ArrowType)]]]

  def defineService(
    name: Ability[S],
    arrows: NonEmptyMap[String, (Name[S], ArrowType)],
    defaultId: Option[ValueRaw]
  ): Alg[Boolean]

  def getArrow(name: Ability[S], arrow: Name[S]): Alg[Option[ArrowType]]

  def setServiceId(name: Ability[S], id: ValueToken[S], vm: ValueRaw): Alg[Boolean]

  def getServiceId(name: Ability[S]): Alg[Either[Boolean, ValueRaw]]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]

}
