package aqua.lsp

import aqua.parser.lexer.Token
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.locations.{LocationsAlgebra, LocationsState, TokenInfo}

import cats.data.State
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging

class LocationsInterpreter[S[_], X](using
  lens: Lens[X, LocationsState[S]]
) extends LocationsAlgebra[S, State[X, *]] with Logging {

  type SX[A] = State[X, A]

  val stack = new StackInterpreter[S, X, LocationsState[S], LocationsState[S]](
    GenLens[LocationsState[S]](_.stack)
  )

  import stack.*

  override def addToken(name: String, tokenInfo: TokenInfo[S]): State[X, Unit] = modify { st =>
    st.copy(tokens = (name, tokenInfo) +: st.tokens)
  }

  private def combineFieldName(name: String, field: String): String = name + "." + field

  override def addTokenWithFields(
    name: String,
    token: TokenInfo[S],
    fields: List[(String, TokenInfo[S])]
  ): State[X, Unit] = {
    val allTokens =
      ((name, token) +: fields.map(kv => (combineFieldName(name, kv._1), kv._2))).toMap
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

  private def findTokenByName(
    st: LocationsState[S],
    name: String,
    token: Token[S]
  ): Option[(Token[S], Token[S])] =
    (st.stack.view.flatMap(_.tokens).collectFirst {
      case (s, t) if s == name => t
    } orElse st.tokens.find(_._1 == name).map(_._2)).map(token -> _.token)

  override def pointLocation(name: String, token: Token[S]): State[X, Unit] = {
    modify { st =>
      val newLoc = findTokenByName(st, name, token)
      st.copy(locations = st.locations ++ newLoc.toList)
    }
  }

  def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] =
    modify { st =>
      val newLocs = locations.flatMap { case (name, token) =>
        findTokenByName(st, name, token)
      }
      st.copy(locations = st.locations ++ newLocs)
    }

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))

  override def beginScope(): SX[Unit] =
    stack.beginScope(LocationsState[S]())

  override def endScope(): SX[Unit] = stack.endScope
}
