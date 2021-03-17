package aqua.ast.algebra.abilities

import aqua.ast.algebra.types.ArrowType
import aqua.ast.gen.ArrowGen
import aqua.parser.lexer.{Ability, Name, Token, Value}
import cats.data.{NonEmptyList, NonEmptyMap}

sealed trait AbilityOp[F[_], T]

case class DefineArrow[F[_]](arrow: Name[F], `type`: ArrowType) extends AbilityOp[F, Boolean]

case class PurgeArrows[F[_]](token: Token[F]) extends AbilityOp[F, Option[NonEmptyList[(Name[F], ArrowType)]]]

case class DefineService[F[_]](name: Ability[F], arrows: NonEmptyMap[String, ArrowGen]) extends AbilityOp[F, Boolean]

case class GetArrow[F[_]](name: Ability[F], arrow: Name[F]) extends AbilityOp[F, Option[ArrowGen]]

case class SetServiceId[F[_]](name: Ability[F], id: Value[F]) extends AbilityOp[F, Boolean]

case class BeginScope[F[_]](token: Token[F]) extends AbilityOp[F, Unit]

case class EndScope[F[_]]() extends AbilityOp[F, Boolean]
