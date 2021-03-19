package aqua.semantics.algebra

import aqua.parser.lexer.Token

trait ReportError[F[_], X] {
  def apply(st: X, token: Token[F], hint: String): X
}
