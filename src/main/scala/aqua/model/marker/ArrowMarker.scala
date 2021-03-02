package aqua.model.marker

import aqua.parser.DefFunc
import aqua.parser.lexer.ArrowType

sealed trait ArrowMarker[F[_], L] extends Marker[F, L]

case class LocalArrow[F[_], L](arr: ArrowType[F]) extends ArrowMarker[F, L]

case class FuncArrow[F[_], L](funcDef: DefFunc[F, L]) extends ArrowMarker[F, L]
