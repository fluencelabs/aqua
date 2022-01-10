package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Ability, Name, Token, Value}
import aqua.raw.value.ValueRaw
import aqua.types.ArrowType
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilitiesAlgebra[S[_], Alg[_]] {

  def defineArrow(arrow: Name[S], `type`: ArrowType): Alg[Boolean]

  def purgeArrows(token: Token[S]): Alg[Option[NonEmptyList[(Name[S], ArrowType)]]]

  def defineService(
    name: Ability[S],
    arrows: NonEmptyMap[String, ArrowType],
    defaultId: Option[ValueRaw]
  ): Alg[Boolean]

  def getArrow(name: Ability[S], arrow: Name[S]): Alg[Option[ArrowType]]

  def setServiceId(name: Ability[S], id: Value[S], vm: ValueRaw): Alg[Boolean]

  def getServiceId(name: Ability[S]): Alg[Either[Boolean, ValueRaw]]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]

}
