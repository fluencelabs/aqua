package aqua.lsp

import aqua.parser.lexer.Token
import aqua.semantics.rules.locations.{ExprInfo, LocationsAlgebra, LocationsState}

import cats.data.State
import monocle.Lens
import scribe.Logging

class LocationsInterpreter[S[_], X](using
  lens: Lens[X, LocationsState[S]]
) extends LocationsAlgebra[S, State[X, *]] with Logging {

  type SX[A] = State[X, A]

  override def addToken(name: String, tokenInfo: ExprInfo[S]): State[X, Unit] = modify { st =>
    st.copy(tokens = (name, tokenInfo) +: st.tokens)
  }

  private def combineFieldName(name: String, field: String): String = name + "." + field

  override def addTokenWithFields(
    name: String,
    token: ExprInfo[S],
    fields: List[(String, ExprInfo[S])]
  ): State[X, Unit] = {
    val allTokens =
      ((name, token) +: fields.map { case (fieldName, info) =>
        combineFieldName(name, fieldName) -> info
      }).toMap
    modify { st =>
      st.copy(tokens = st.tokens ++ allTokens)
    }
  }

  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): State[X, Unit] =
    pointLocation(combineFieldName(typeName, fieldName), token)

  def pointTokenWithFieldLocation(
    typeName: String,
    typeToken: Token[S],
    fieldName: String,
    token: Token[S]
  ): State[X, Unit] = {
    for {
      _ <- pointLocation(typeName, typeToken)
      _ <- pointLocation(combineFieldName(typeName, fieldName), token)
    } yield {}
  }

  override def pointLocation(name: String, token: Token[S]): State[X, Unit] = {
    modify { st =>
      val newLoc = st.findTokenByName(name, token)
      st.copy(locations = st.locations ++ newLoc.toList)
    }
  }

  def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] =
    modify { st =>
      val newLocs = locations.flatMap { case (name, token) =>
        st.findTokenByName(name, token)
      }
      st.copy(locations = st.locations ++ newLocs)
    }

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))
}
