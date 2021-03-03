package aqua.context.marker

import aqua.parser.AbilityResolve
import aqua.parser.lexer.Token

trait AbilityResolveMarker[F[_]] extends Marker[F]

case class ResolvedMarker[F[_], L](resolve: AbilityResolve[F, L]) extends AbilityResolveMarker[F] {
  override def pointer: Token[F] = resolve.ability
}
