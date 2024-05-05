package aqua.semantics.rules.locations

import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.Monoid
import cats.syntax.semigroup.*
import scribe.Logging

case class LocationsState[S[_]](
  variables: Variables[S] = Variables[S]()
) extends Logging {

  lazy val allLocations: List[TokenLocation[S]] = variables.allLocations

  def addDefinitions(newDefinitions: List[DefinitionInfo[S]]): LocationsState[S] = {
    copy(variables = variables.addDefinitions(newDefinitions))
  }

  def addDefinition(newDef: DefinitionInfo[S]): LocationsState[S] =
    copy(variables = variables.addDefinitions(newDef :: Nil))

  def addLocation(
    name: String,
    token: Token[S]
  ): LocationsState[S] =
    copy(variables = variables.addLocation(name, token))

  def addLocations(
    locations: List[(String, Token[S])]
  ): LocationsState[S] =
    locations.foldLeft(this) { case (st, (name, token)) =>
      st.addLocation(name, token)
    }
}

object LocationsState {

  given [S[_]]: Monoid[LocationsState[S]] with {
    override def empty: LocationsState[S] = LocationsState[S]()

    override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
      LocationsState(
        variables = x.variables.combine(y.variables)
      )
  }
}
