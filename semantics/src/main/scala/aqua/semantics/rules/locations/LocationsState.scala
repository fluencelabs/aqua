package aqua.semantics.rules.locations

import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.Monoid
import scribe.Logging

case class LocationsState[S[_]](
  variables: List[VariableInfo[S]] = Nil
) extends Logging {

  def addDefinitions(newDefinitions: List[DefinitionInfo[S]]): LocationsState[S] =
    copy(variables = newDefinitions.map(d => VariableInfo(d)) ++ variables)

  def addDefinition(newDef: DefinitionInfo[S]): LocationsState[S] =
    copy(variables = VariableInfo(newDef) +: variables)

  private def addOccurrenceToFirst(
    vars: List[VariableInfo[S]],
    name: String,
    token: Token[S]
  ): List[VariableInfo[S]] = {
    // TODO: this code lasts too long, but we can find errors in it.
    // if (!vars.exists(_.definition.name == name))
    //   logger.error(s"Unexpected. Cannot add occurrence for $name")

    vars.updateFirst(_.definition.name == name, v => v.copy(occurrences = token +: v.occurrences))
  }

  def addLocation(
    name: String,
    token: Token[S]
  ): LocationsState[S] =
    copy(variables = addOccurrenceToFirst(variables, name, token))

  def addLocations(
    locations: List[(String, Token[S])]
  ): LocationsState[S] =
    locations.foldLeft(this) { case (st, (name, token)) =>
      st.addLocation(name, token)
    }
}

object LocationsState {

  implicit def locationsStateMonoid[S[_]]: Monoid[LocationsState[S]] =
    new Monoid[LocationsState[S]] {
      override def empty: LocationsState[S] = LocationsState()

      override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
        LocationsState(
          variables = x.variables ++ y.variables
        )
    }
}
