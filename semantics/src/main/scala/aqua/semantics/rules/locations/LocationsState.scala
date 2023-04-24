package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType, TokenTypeInfo}

case class LocationsState[S[_]](
  tokenDefinitions: Map[String, TokenTypeInfo[S]] = Map.empty[String, TokenTypeInfo[S]],
  nameLocations: List[(Token[S], TokenType[S])] = Nil,
  typeLocations: List[(Token[S], TokenInfo[S])] = Nil,
  serviceLocations: List[(Token[S], TokenInfo[S])] = Nil
) {

  lazy val allLocations: List[(Token[S], TokenInfo[S])] =
    nameLocations ++ typeLocations ++ serviceLocations
}
