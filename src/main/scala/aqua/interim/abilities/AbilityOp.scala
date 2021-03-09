package aqua.interim.abilities

import aqua.interim.types.ArrowType
import aqua.parser.lexer.{Ability, Name, Token, Value}
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilityOp[T]

case class DefineArrow[F[_]](arrow: Name[F], `type`: ArrowType) extends AbilityOp[Unit]
case class PurgeArrows[F[_]]() extends AbilityOp[NonEmptyList[(Name[F], ArrowType)]]
case class DefineService[F[_]](name: Ability[F], arrows: NonEmptyMap[String, ArrowType]) extends AbilityOp[Unit]
case class GetArrow[F[_]](name: Ability[F], arrow: Name[F]) extends AbilityOp[ArrowType]
case class SetServiceId[F[_]](name: Ability[F], id: Value[F]) extends AbilityOp[Unit]

case class BeginScope[F[_]](token: Token[F]) extends AbilityOp[Unit]
case class EndScope[F[_]]() extends AbilityOp[Unit]
