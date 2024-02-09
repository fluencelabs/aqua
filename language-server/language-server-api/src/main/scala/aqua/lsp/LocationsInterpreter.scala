package aqua.lsp

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

  override def addDefinition(definition: DefinitionInfo[S]): State[X, Unit] = modify { st =>
    st.addDefinition(definition)
  }

  override def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): State[X, Unit] = {
    val allTokens =
      definition +: fields.map { fieldDef =>
        fieldDef.copy(name = AbilityType.fullName(definition.name, fieldDef.name))
      }
    modify { st =>
      st.addDefinitions(allTokens)
    }
  }

  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): State[X, Unit] =
    pointLocation(AbilityType.fullName(typeName, fieldName), token)

  def pointTokenWithFieldLocation(
    typeName: String,
    typeToken: Token[S],
    fieldName: String,
    token: Token[S]
  ): State[X, Unit] = {
    for {
      _ <- pointLocation(typeName, typeToken)
      _ <- pointLocation(AbilityType.fullName(typeName, fieldName), token)
    } yield {}
  }

  override def pointLocation(name: String, token: Token[S]): State[X, Unit] = {
    modify { st =>
      st.addLocation(name, token)
    }
  }

  def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] =
    modify { st =>
      println("add locations: " + locations)
      st.addLocations(locations)
    }

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))
}
