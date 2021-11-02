package aqua.semantics.rules.abilities

import aqua.model.ValueModel
import aqua.parser.lexer.{Ability, Name, Token, Value}
import aqua.types.ArrowType
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilitiesAlgebra[F[_], Alg[_]] {

  def defineArrow(arrow: Name[F], `type`: ArrowType): Alg[Boolean]

  def purgeArrows(token: Token[F]): Alg[Option[NonEmptyList[(Name[F], ArrowType)]]]

  def defineService(
    name: Ability[F],
    arrows: NonEmptyMap[String, ArrowType],
    defaultId: Option[ValueModel]
  ): Alg[Boolean]

  def getArrow(name: Ability[F], arrow: Name[F]): Alg[Option[ArrowType]]

  def setServiceId(name: Ability[F], id: Value[F], vm: ValueModel): Alg[Boolean]

  def getServiceId(name: Ability[F]): Alg[Either[Boolean, ValueModel]]

  def beginScope(token: Token[F]): Alg[Unit]

  def endScope(): Alg[Unit]

}
