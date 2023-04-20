package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType}
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.types.TypesState
import monocle.Lens
import monocle.macros.GenLens
import cats.data.{NonEmptyList, NonEmptyMap, State}

class DummyLocationsInterpreter[S[_], X] extends LocationsAlgebra[S, State[X, *]] {

  override def addNameLocations(locs: List[(Token[S], TokenType[S])]): State[X, Unit] = State.pure(())

  override def addNameLocation(token: Token[S], tokenType: TokenType[S]): State[X, Unit] = State.pure(())

  override def addTypeLocations(locs: List[(Token[S], TokenInfo[S])]): State[X, Unit] = State.pure(())

  override def addTypeLocation(token: Token[S], tokenInfo: TokenInfo[S]): State[X, Unit] = State.pure(())

  override def addServiceLocations(locs: List[(Token[S], TokenInfo[S])]): State[X, Unit] = State.pure(())

  override def addServiceLocation(token: Token[S], tokenInfo: TokenInfo[S]): State[X, Unit] = State.pure(())
}
