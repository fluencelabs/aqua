package aqua.parser.head

import aqua.parser.lexer.{Literal, Token}
import cats.Comonad
import cats.~>

trait FilenameExpr[F[_]] extends HeaderExpr[F] {
  def filename: Literal[F]

  override def token: Token[F] = filename

  def fileValue: String = filename.value.drop(1).dropRight(1)

  override def mapK[K[_]: Comonad](fk: F ~> K): FilenameExpr[K]
    
}
