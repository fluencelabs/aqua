package aqua.parser

import aqua.parser.lexer.{ArrowType, Token}
import cats.Functor

sealed trait ArrowMarker[F[_]] extends Token[F]

case class LocalArrow[F[_]](arr: ArrowType[F]) extends ArrowMarker[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = arr.as(v)
}

case class FuncArrow[F[_]](funcDef: DefFunc[F]) extends ArrowMarker[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = funcDef.as(v)
}
