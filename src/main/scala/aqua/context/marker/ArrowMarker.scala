package aqua.context.marker

import aqua.parser.DefFunc
import aqua.parser.lexer.{Ability, ArrowType, Token}

sealed trait ArrowMarker[F[_]] extends Marker[F]

case class LocalArrow[F[_], L](arr: ArrowType[F]) extends ArrowMarker[F] {
  override def pointer: Token[F] = arr
}

case class FuncArrow[F[_], L](funcDef: DefFunc[F, L]) extends ArrowMarker[F] {
  override def pointer: Token[F] = funcDef.head.name
}

case class AbilityArrow[F[_], L](ability: Ability[F], arr: ArrowType[F]) extends ArrowMarker[F] {
  override def pointer: Token[F] = arr
}
