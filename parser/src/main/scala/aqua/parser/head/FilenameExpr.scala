package aqua.parser.head

import aqua.parser.lexer.Literal

trait FilenameExpr[F[_]] extends HeaderExpr[F] {
  def filename: Literal[F]

  def fileValue: String = filename.value.drop(1).dropRight(1)
}
