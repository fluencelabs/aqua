package aqua.semantics.rules.locations

import aqua.helpers.data.PName
import aqua.helpers.data.SName
import aqua.parser.lexer.Token

import cats.data.State

class DummyLocationsInterpreter[S[_], X] extends LocationsAlgebra[S, State[X, *]] {

  def addDefinition(definition: DefinitionInfo[S]): State[X, Unit] = State.pure(())

  def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): State[X, Unit] = State.pure(())

  def pointFieldLocation(
    typeName: PName,
    fieldName: SName,
    token: Token[S]
  ): State[X, Unit] = State.pure(())

  def pointTokenWithFieldLocation(
    typeName: PName,
    typeToken: Token[S],
    fieldName: SName,
    token: Token[S]
  ): State[X, Unit] = State.pure(())
  override def pointLocation(name: PName, token: Token[S]): State[X, Unit] = State.pure(())
  override def pointLocations(locations: List[(PName, Token[S])]): State[X, Unit] = State.pure(())

  def beginScope(): State[X, Unit] = State.pure(())

  def endScope(): State[X, Unit] = State.pure(())
}
