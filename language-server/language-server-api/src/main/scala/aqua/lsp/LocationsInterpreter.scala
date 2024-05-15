package aqua.lsp

import aqua.helpers.data.PName
import aqua.helpers.data.SName
import aqua.parser.lexer.Token
import aqua.semantics.rules.locations.{DefinitionInfo, LocationsAlgebra, LocationsState}
import aqua.types.AbilityType

import cats.data.State
import monocle.Lens
import scribe.Logging

class LocationsInterpreter[S[_], X](using
  lens: Lens[X, LocationsState[S]]
) extends LocationsAlgebra[S, State[X, *]] with Logging {

  type SX[A] = State[X, A]

  override def addDefinition(definition: DefinitionInfo[S]): State[X, Unit] =
    definition.`type` match {
      // case where ability is an {Argument} in a function
      case t: AbilityType if definition.name.value == t.name =>
        pointLocation(definition.name, definition.token)
      case _ => modify { st => st.addDefinition(definition) }
    }

  override def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): State[X, Unit] =
    modify(_.addDefinitions(definition +: fields.map { fieldDef =>
      fieldDef.copy(name = fieldDef.name.prepended(definition.name))
    }))

  override def pointFieldLocation(
    typeName: PName,
    fieldName: SName,
    token: Token[S]
  ): State[X, Unit] =
    pointLocation(typeName.postfixed(fieldName), token)

  override def pointTokenWithFieldLocation(
    typeName: PName,
    typeToken: Token[S],
    fieldName: SName,
    token: Token[S]
  ): State[X, Unit] = {
    for {
      _ <- pointLocation(typeName, typeToken)
      _ <- pointLocation(typeName.postfixed(fieldName), token)
    } yield {}
  }

  override def pointLocation(name: PName, token: Token[S]): State[X, Unit] =
    modify(_.addLocation(name, token))

  override def pointLocations(locations: List[(PName, Token[S])]): State[X, Unit] =
    modify(_.addLocations(locations))

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))
}
