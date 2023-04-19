package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType}
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.types.TypesState
import monocle.Lens
import monocle.macros.GenLens
import cats.data.{NonEmptyList, NonEmptyMap, State}

class LocationsInterpreter[S[_], X](implicit
  lens: Lens[X, LocationsState[S]]
) extends LocationsAlgebra[S, State[X, *]] {

  type SX[A] = State[X, A]

  private def getState = State.get.map(lens.get)

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))

  override def addNameLocations(locs: List[(Token[S], TokenType[S])]): State[X, Unit] =
    modify(st => st.copy(nameLocations = st.nameLocations ++ locs))

  override def addNameLocation(token: Token[S], tokenType: TokenType[S]): State[X, Unit] =
    modify(st => st.copy(nameLocations = st.nameLocations :+ (token, tokenType)))

  override def addTypeLocations(locs: List[(Token[S], TokenInfo[S])]): State[X, Unit] =
    modify(st => st.copy(typeLocations = st.nameLocations ++ locs))

  override def addTypeLocation(token: Token[S], tokenInfo: TokenInfo[S]): State[X, Unit] =
    modify(st => st.copy(typeLocations = st.typeLocations :+ (token, tokenInfo)))

  override def addServiceLocations(locs: List[(Token[S], TokenInfo[S])]): State[X, Unit] =
    modify(st => st.copy(serviceLocations = st.nameLocations ++ locs))

  override def addServiceLocation(token: Token[S], tokenInfo: TokenInfo[S]): State[X, Unit] =
    modify(st => st.copy(serviceLocations = st.serviceLocations :+ (token, tokenInfo)))
}
