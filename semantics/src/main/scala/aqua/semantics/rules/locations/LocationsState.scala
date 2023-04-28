package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenDef, TokenInfo, TokenType, TokenTypeInfo}
import aqua.semantics.rules.types.TypesState
import cats.kernel.Monoid

case class LocationsState[S[_]](
  tokens: Map[String, Token[S]] = Map.empty[String, Token[S]],
  locations: List[(Token[S], Token[S])] = Nil,
  stack: List[LocationsState[S]] = Nil,
  fieldsTokens: Map[String, TokenTypeInfo[S]] = Map.empty[String, TokenTypeInfo[S]]
) {

  lazy val allLocations: List[(Token[S], TokenInfo[S])] =
    locations.map(tt => tt._1 -> TokenDef[S](Option(tt._2)))
}

object LocationsState {

  implicit def locationsStateMonoid[S[_]]: Monoid[LocationsState[S]] = new Monoid[LocationsState[S]] {
    override def empty: LocationsState[S] = LocationsState()

    override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
      println(s"combine ${x.tokens.keys} with ${y.tokens.keys}")
      LocationsState(
        tokens = x.tokens ++ y.tokens
      )
  }
}
