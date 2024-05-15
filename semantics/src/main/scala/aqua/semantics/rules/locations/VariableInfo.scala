package aqua.semantics.rules.locations

import aqua.helpers.data.PName
import aqua.parser.lexer.Token
import aqua.types.Type

import monocle.Lens
import monocle.macros.GenLens

case class DefinitionInfo[S[_]](name: PName, token: Token[S], `type`: Type)
case class TokenLocation[S[_]](usage: Token[S], definition: Token[S])

case class VariableInfo[S[_]](
  definition: DefinitionInfo[S],
  occurrences: List[Token[S]] = Nil
) {

  def isFor(name: PName): Boolean =
    definition.name == name

  def rename(newName: PName): VariableInfo[S] =
    VariableInfo.definitionNameLens[S].set(newName)(this)

  def addOccurence(token: Token[S]): VariableInfo[S] =
    VariableInfo.occurrancesLens[S].modify(token :: _)(this)

  def locations: List[TokenLocation[S]] =
    occurrences.map(o => TokenLocation(o, definition.token))
}

object VariableInfo {

  def occurrancesLens[S[_]]: Lens[VariableInfo[S], List[Token[S]]] =
    GenLens[VariableInfo[S]](_.occurrences)

  def definitionNameLens[S[_]]: Lens[VariableInfo[S], PName] =
    GenLens[VariableInfo[S]](_.definition.name)
}
