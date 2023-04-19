package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType}
import monocle.Lens
import monocle.macros.GenLens
import cats.data.{NonEmptyList, NonEmptyMap, State}

class LocationsInterpreter[S[_], X](implicit
  lens: Lens[X, LocationsState[S]]
) extends LocationsAlgebra[S, State[X, *]] {

  type SX[A] = State[X, A]

  override def addNameLocation(token: Token[S], tokenType: TokenType[S]): State[X, Unit] = ???
  override def addTypeLocation(token: Token[S], tokenType: TokenInfo[S]): State[X, Unit] = ???
  override def addServiceLocation(token: Token[S], tokenType: TokenInfo[S]): State[X, Unit] = ???
}
