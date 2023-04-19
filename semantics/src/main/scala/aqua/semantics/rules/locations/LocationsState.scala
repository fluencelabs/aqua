package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.lsp.TokenType

case class LocationsState[S[_]](
  locations: List[(Token[S], TokenType[S])] = Nil
)
