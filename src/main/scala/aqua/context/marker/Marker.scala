package aqua.context.marker

import aqua.parser.lexer.Token
import cats.Functor

trait Marker[F[_]] {
  def pointer: Token[F]

  def toError(str: String)(implicit F: Functor[F]): F[String] = pointer.as(str)
}
