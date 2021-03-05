package aqua.context.marker

import aqua.parser.DefFunc
import aqua.parser.lexer.{Ability, ArrowDef, ArrowTypeToken, Token}

sealed trait ArrowMarker[F[_]] extends Marker[F] {
  def arrowDef: ArrowDef[F]
}

case class LocalArrow[F[_], L](arr: ArrowTypeToken[F]) extends ArrowMarker[F] {
  override def pointer: Token[F] = arr

  override def arrowDef: ArrowDef[F] = arr
}

case class FuncArrow[F[_], L](funcDef: DefFunc[F, L]) extends ArrowMarker[F] {
  override def pointer: Token[F] = funcDef.head.name

  override def arrowDef: ArrowDef[F] = funcDef.head.arrowDef
}

case class AbilityArrow[F[_], L](ability: Ability[F], arr: ArrowTypeToken[F]) extends ArrowMarker[F] {
  override def pointer: Token[F] = arr

  override def arrowDef: ArrowDef[F] = arr
}
