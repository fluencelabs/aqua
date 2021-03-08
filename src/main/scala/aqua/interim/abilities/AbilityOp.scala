package aqua.interim.abilities

import aqua.interim.types.ArrowType
import aqua.parser.lexer.{Ability, ArrowName, Value}
import cats.data.NonEmptyMap

trait AbilityOp[T]

case class DefineArrow[F[_]](arrow: ArrowName[F], `type`: ArrowType) extends AbilityOp[Unit]
case class DefineService[F[_]](name: Ability[F], arrows: NonEmptyMap[String, ArrowType]) extends AbilityOp[Unit]
case class GetArrow[F[_]](name: Ability[F], arrow: ArrowName[F]) extends AbilityOp[ArrowType]
case class SetServiceId[F[_]](name: Ability[F], id: Value[F]) extends AbilityOp[Unit]
case class UnsetServiceId[F[_]](name: Ability[F]) extends AbilityOp[Unit]
