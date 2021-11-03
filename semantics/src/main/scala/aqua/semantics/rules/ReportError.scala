package aqua.semantics.rules

import aqua.parser.lexer.Token

trait ReportError[S[_], X] {
  def apply(st: X, token: Token[S], hint: String): X
}
