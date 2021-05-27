package aqua.semantics.rules.abilities

import aqua.model.ValueModel
import aqua.parser.lexer.{Ability, Name, Token, Value}
import aqua.types.ArrowType
import cats.data.{NonEmptyList, NonEmptyMap}

sealed trait AbilityOp[F[_], T]

case class DefineArrow[F[_]](arrow: Name[F], `type`: ArrowType) extends AbilityOp[F, Boolean]

case class PurgeArrows[F[_]](token: Token[F])
    extends AbilityOp[F, Option[NonEmptyList[(Name[F], ArrowType)]]]

case class DefineService[F[_]](
  name: Ability[F],
  arrows: NonEmptyMap[String, ArrowType],
  v: Option[ValueModel]
) extends AbilityOp[F, Boolean]

case class GetArrow[F[_]](name: Ability[F], arrow: Name[F]) extends AbilityOp[F, Option[ArrowType]]

case class SetServiceId[F[_]](name: Ability[F], id: Value[F]) extends AbilityOp[F, Boolean]

case class GetServiceId[F[_]](name: Ability[F]) extends AbilityOp[F, Option[ValueModel]]

case class BeginScope[F[_]](token: Token[F]) extends AbilityOp[F, Unit]

case class EndScope[F[_]]() extends AbilityOp[F, Boolean]
