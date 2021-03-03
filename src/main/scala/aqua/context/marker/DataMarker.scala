package aqua.context.marker

import aqua.parser.Extract
import aqua.parser.lexer.{DataType, Var}
import cats.Functor

sealed trait DataMarker[F[_], L] extends Marker[F, L] {
  def toError(str: String)(implicit F: Functor[F]): F[String]
}

case class FuncArgMarker[F[_], L](v: Var[F], dt: DataType[F]) extends DataMarker[F, L] {
  override def toError(str: String)(implicit F: Functor[F]): F[String] = v.as(str)
}

case class ExtractedVarMarker[F[_], L](v: Var[F], ext: Extract[F, _]) extends DataMarker[F, L] {
  override def toError(str: String)(implicit F: Functor[F]): F[String] = v.as(str)
}
