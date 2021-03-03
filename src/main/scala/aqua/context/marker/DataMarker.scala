package aqua.context.marker

import aqua.parser.Extract
import aqua.parser.lexer.{DataType, Token, Var}
import cats.Functor

sealed trait DataMarker[F[_]] extends Marker[F]

case class FuncArgMarker[F[_], L](v: Var[F], dt: DataType[F]) extends DataMarker[F] {
  override def pointer: Token[F] = v
}

case class ExtractedVarMarker[F[_], L](v: Var[F], ext: Extract[F, L]) extends DataMarker[F] {
  override def pointer: Token[F] = v
}
