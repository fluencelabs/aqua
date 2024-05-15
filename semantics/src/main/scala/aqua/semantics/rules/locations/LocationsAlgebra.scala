package aqua.semantics.rules.locations

import aqua.helpers.data.PName
import aqua.helpers.data.SName
import aqua.parser.lexer.Token
import aqua.types.Type

trait LocationsAlgebra[S[_], Alg[_]] {
  def addDefinition(definition: DefinitionInfo[S]): Alg[Unit]

  def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): Alg[Unit]

  def pointTokenWithFieldLocation(
    typeName: PName,
    typeToken: Token[S],
    fieldName: SName,
    token: Token[S]
  ): Alg[Unit]

  def pointFieldLocation(
    typeName: PName,
    fieldName: SName,
    token: Token[S]
  ): Alg[Unit]

  def pointLocation(name: PName, token: Token[S]): Alg[Unit]
  def pointLocations(locations: List[(PName, Token[S])]): Alg[Unit]
}
