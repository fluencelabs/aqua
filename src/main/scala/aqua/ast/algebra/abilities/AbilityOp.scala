package aqua.ast.algebra.abilities

import aqua.ast.algebra.types.ArrowType
import aqua.parser.lexer.{Ability, Name, Token, Value}
import cats.data.{NonEmptyList, NonEmptyMap}

sealed trait AbilityOp[T] {
  type CF[_]
}

object AbilityOp {

  type Aux[F[_], T] = AbilityOp[T] {
    type CF[A] = F[A]
  }

  def defineService[F[_]](name: Ability[F], arrows: NonEmptyMap[String, ArrowType]): AbilityOp.Aux[F, Unit] =
    DefineService[F](name, arrows)
}

case class DefineArrow[F[_]](arrow: Name[F], `type`: ArrowType) extends AbilityOp[Unit] {
  type CF[A] = F[A]
}

case class PurgeArrows[F[_]]() extends AbilityOp[NonEmptyList[(Name[F], ArrowType)]] {
  type CF[A] = F[A]
}

case class DefineService[F[_]](name: Ability[F], arrows: NonEmptyMap[String, ArrowType]) extends AbilityOp[Unit] {
  type CF[A] = F[A]
}

case class GetArrow[F[_]](name: Ability[F], arrow: Name[F]) extends AbilityOp[ArrowType] {
  type CF[A] = F[A]
}

case class SetServiceId[F[_]](name: Ability[F], id: Value[F]) extends AbilityOp[Unit] {
  type CF[A] = F[A]
}

case class BeginScope[F[_]](token: Token[F]) extends AbilityOp[Unit] {
  type CF[A] = F[A]

  def t: Token[CF] = token
}

case class EndScope[F[_]]() extends AbilityOp[Unit] {
  type CF[A] = F[A]
}
