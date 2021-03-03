package aqua.context.marker

import aqua.parser.DefService
import aqua.parser.lexer.{Ability, Token}

sealed trait AbilityMarker[F[_]] extends Marker[F]

case class ServiceAbility[F[_], L](ability: Ability[F], service: DefService[F, L]) extends AbilityMarker[F] {
  override def pointer: Token[F] = ability
}
