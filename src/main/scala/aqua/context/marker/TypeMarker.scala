package aqua.context.marker

import aqua.parser.DefType
import aqua.parser.lexer.Type

sealed trait TypeMarker[F[_], L] extends Marker[F, L]

case class TypeAlias[F[_], L](forType: Type[F]) extends TypeMarker[F, L]

case class TypeDef[F[_], L](forDef: DefType[F, L]) extends TypeMarker[F, L]
