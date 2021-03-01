package aqua.parser

import aqua.parser.lexer.{Token, Type}
import cats.Functor

sealed trait TypeMarker[F[_]] extends Token[F]

case class TypeAlias[F[_]](forType: Type[F]) extends TypeMarker[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = forType.as(v)
}

case class TypeDef[F[_]](forDef: DefType[F]) extends TypeMarker[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = forDef.as(v)
}
