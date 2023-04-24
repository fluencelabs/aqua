package aqua.semantics.rules.definitions

import aqua.parser.lexer.{Name, Token}
import aqua.semantics.lsp.TokenTypeInfo
import aqua.types.Type

case class DefinitionsState[S[_]](
  definitions: Map[String, (Name[S], Type)] = Map.empty[String, (Name[S], Type)],
  definitionsTokens: Map[String, TokenTypeInfo[S]] = Map.empty[String, TokenTypeInfo[S]]
)
