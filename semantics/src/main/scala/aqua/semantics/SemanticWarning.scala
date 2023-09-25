package aqua.semantics

import aqua.parser.lexer.Token

final case class SemanticWarning[S[_]](
  token: Token[S],
  hints: List[String]
)
