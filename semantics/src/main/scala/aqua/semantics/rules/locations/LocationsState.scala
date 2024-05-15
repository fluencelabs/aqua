package aqua.semantics.rules.locations

import aqua.helpers.data.PName
import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.Monoid
import cats.syntax.semigroup.*
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging

case class LocationsState[S[_]](
  variables: Variables[S] = Variables[S]()
) extends Logging {

  lazy val locations: List[TokenLocation[S]] = variables.locations

  def addDefinitions(newDefinitions: List[DefinitionInfo[S]]): LocationsState[S] =
    modifyVariables(_.addDefinitions(newDefinitions))

  def addDefinition(newDef: DefinitionInfo[S]): LocationsState[S] =
    addDefinitions(List(newDef))

  def addLocation(
    name: PName,
    token: Token[S]
  ): LocationsState[S] =
    modifyVariables(_.addOccurence(name, token))

  def addLocations(
    locations: List[(PName, Token[S])]
  ): LocationsState[S] =
    locations.foldLeft(this) { case (st, (name, token)) =>
      st.addLocation(name, token)
    }

  private def modifyVariables(f: Variables[S] => Variables[S]): LocationsState[S] =
    LocationsState.variablesLens[S].modify(f)(this)
}

object LocationsState {

  def variablesLens[S[_]]: Lens[LocationsState[S], Variables[S]] =
    GenLens[LocationsState[S]](_.variables)

  given [S[_]]: Monoid[LocationsState[S]] with {
    override def empty: LocationsState[S] = LocationsState[S]()

    override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
      LocationsState(
        variables = x.variables.combine(y.variables)
      )
  }
}
