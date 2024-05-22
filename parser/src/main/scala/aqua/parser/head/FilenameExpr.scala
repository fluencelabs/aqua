package aqua.parser.head

import aqua.helpers.ext.Extension
import aqua.parser.lexer.{LiteralToken, Token}
import cats.Comonad
import cats.~>

trait FilenameExpr[F[_]] extends HeaderExpr[F] {
  def filename: LiteralToken[F]

  override def token: Token[F] = filename

  def fileValue: String = {
    val raw = filename.value.drop(1).dropRight(1)
    Extension.add(raw)
  }

  override def mapK[K[_]: Comonad](fk: F ~> K): FilenameExpr[K]
}
