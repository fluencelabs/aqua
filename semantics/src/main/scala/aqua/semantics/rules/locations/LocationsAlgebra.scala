package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.types.Type

trait LocationsAlgebra[S[_], Alg[_]] {
  def addDefinition(definition: DefinitionInfo[S]): Alg[Unit]

  def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): Alg[Unit]

  def pointTokenWithFieldLocation(
    typeName: String,
    typeToken: Token[S],
    fieldName: String,
    token: Token[S]
  ): Alg[Unit]
  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): Alg[Unit]
  def pointLocation(name: String, token: Token[S]): Alg[Unit]
  def pointLocations(locations: List[(String, Token[S])]): Alg[Unit]
}
