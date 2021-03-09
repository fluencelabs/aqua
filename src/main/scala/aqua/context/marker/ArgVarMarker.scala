package aqua.context.marker

import aqua.parser.Extract
import aqua.parser.lexer.{DataTypeToken, Name, Token}

sealed trait ArgVarMarker[F[_]] extends Marker[F]

case class FuncArgMarker[F[_]](v: Name[F], dt: DataTypeToken[F]) extends ArgVarMarker[F] {
  override def pointer: Token[F] = v
}

case class ExtractedVarMarker[F[_], L](v: Name[F], extract: Extract[F, L]) extends ArgVarMarker[F] {
  override def pointer: Token[F] = v
}
