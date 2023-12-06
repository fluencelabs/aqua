package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.types.Type

case class DefinitionInfo[S[_]](name: String, token: Token[S], `type`: Type)
case class TokenLocation[S[_]](usage: Token[S], definition: Token[S])

case class VariableInfo[S[_]](definition: DefinitionInfo[S], occurrences: List[Token[S]] = Nil) {
  def allLocations: List[TokenLocation[S]] = occurrences.map(o => TokenLocation(o, definition.token))
}
