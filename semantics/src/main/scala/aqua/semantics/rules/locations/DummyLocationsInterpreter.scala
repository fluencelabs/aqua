package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import cats.data.State

class DummyLocationsInterpreter[S[_], X] extends LocationsAlgebra[S, State[X, *]] {

  def addDefinition(definition: DefinitionInfo[S]): State[X, Unit] = State.pure(())

  def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): State[X, Unit] = State.pure(())

  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): State[X, Unit] =
    State.pure(())

  def pointTokenWithFieldLocation(
    typeName: String,
    typeToken: Token[S],
    fieldName: String,
    token: Token[S]
  ): State[X, Unit] = State.pure(())
  override def pointLocation(name: String, token: Token[S]): State[X, Unit] = State.pure(())
  override def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] = State.pure(())

  def beginScope(): State[X, Unit] = State.pure(())

  def endScope(): State[X, Unit] = State.pure(())
}
