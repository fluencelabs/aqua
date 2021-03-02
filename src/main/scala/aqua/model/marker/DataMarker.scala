package aqua.model.marker

import aqua.parser.lexer.Var
import cats.Functor

sealed trait DataMarker[F[_], L] extends Marker[F, L] {
  def toError(str: String)(implicit F: Functor[F]): F[String]
}

case class VarMarker[F[_], L](v: Var[F]) extends DataMarker[F, L] {
  override def toError(str: String)(implicit F: Functor[F]): F[String] = v.as(str)
}
