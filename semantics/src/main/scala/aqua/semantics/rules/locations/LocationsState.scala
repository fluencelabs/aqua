package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.rules.types.TypesState
import cats.kernel.Monoid

case class LocationsState[S[_]](
  tokens: Map[String, TokenInfo[S]] = Map.empty[String, TokenInfo[S]],
  locations: List[(Token[S], Token[S])] = Nil,
  stack: List[LocationsState[S]] = Nil
) {

  lazy val allLocations: List[(Token[S], Token[S])] = locations
}

object LocationsState {

  implicit def locationsStateMonoid[S[_]]: Monoid[LocationsState[S]] = new Monoid[LocationsState[S]] {
    override def empty: LocationsState[S] = LocationsState()

    override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
      LocationsState(
        tokens = x.tokens ++ y.tokens
      )
  }
}
