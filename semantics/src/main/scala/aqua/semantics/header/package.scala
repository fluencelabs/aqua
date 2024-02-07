package aqua.semantics

import cats.data.State

import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.parser.lexer.Token

package object header {

  /*
   NOTE: This extension glues locations algebra from the body semantics
         with the context that is used in the header semantics 
   */
  extension [S[_], C](context: C)(using
    locations: LocationsAlgebra[S, State[C, *]]
  ) {
    def addOccurences(tokens: List[(String, Token[S])]): C = 
        locations.pointLocations(tokens).runS(context).value
  }
}
