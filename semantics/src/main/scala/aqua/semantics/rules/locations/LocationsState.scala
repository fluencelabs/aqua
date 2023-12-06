package aqua.semantics.rules.locations

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
  ): List[VariableInfo[S]] = vars match {
    case Nil =>
      logger.error(s"Unexpected. Cannot add occurrence for $name")
      Nil
    case head :: tail =>
      if (head.definition.name == name)
        head.copy(occurrences = token +: head.occurrences) :: tail
      else
        head :: addOccurrenceToFirst(tail, name, token)
  }

  def addLocation(
    name: String,
    token: Token[S]
  ): LocationsState[S] = {
    copy(variables = addOccurrenceToFirst(variables, name, token))
  }

  def addLocations(
    locations: List[(String, Token[S])]
  ): LocationsState[S] =
    locations.foldLeft(this) { case (st, (name, token)) =>
      st.copy(variables = addOccurrenceToFirst(variables, name, token))
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
