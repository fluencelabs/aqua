package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.rules.types.TypesState

import cats.kernel.Monoid

case class LocationsState[S[_]](
  tokens: List[(String, ExprInfo[S])] = Nil,
  locations: List[(Token[S], Token[S])] = Nil
) {

  lazy val allLocations: List[(Token[S], Token[S])] = locations

  def findTokenByName(
    name: String,
    token: Token[S]
  ): Option[(Token[S], Token[S])] =
    tokens.find(_._1 == name).map(_._2).map(token -> _.token)
}

object LocationsState {

  implicit def locationsStateMonoid[S[_]]: Monoid[LocationsState[S]] =
    new Monoid[LocationsState[S]] {
      override def empty: LocationsState[S] = LocationsState()

      override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
        LocationsState(
          tokens = x.tokens ++ y.tokens
        )
    }
}
