package aqua.lsp

import aqua.parser.lexer.Token
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.locations.{LocationsAlgebra, LocationsState, TokenInfo}
import aqua.types.{BottomType, Type}

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

  override def addToken(name: String, tokenInfo: TokenInfo[S]): State[X, Unit] = modify {
    st =>
      st.copy(tokens = st.tokens.updated(name, tokenInfo))
  }

  private def combineFieldName(name: String, field: String): String = name + "." + field

  override def addTokenWithFields(
    name: String,
    token: Token[S],
    fields: List[(String, Token[S])]
  ): State[X, Unit] = modify { st =>
    st.copy(tokens =
      st.tokens ++ ((name, TokenInfo(token, BottomType)) +: fields.map(kv =>
        (combineFieldName(name, kv._1), TokenInfo(kv._2, BottomType))
      )).toMap
    )
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
      val newLoc: Option[Token[S]] = st.stack.collectFirst {
        case frame if frame.tokens.contains(name) => frame.tokens(name).token
      } orElse st.tokens.get(name).map(_.token)
      st.copy(locations = st.locations ++ newLoc.map(token -> _).toList)
    }
  }

  def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] = {
    modify { st =>

      val newLocs = locations.flatMap { case (name, token) =>
        (st.stack.collectFirst {
          case frame if frame.tokens.contains(name) => frame.tokens(name).token
        } orElse st.tokens.get(name).map(_.token)).map(token -> _)
      }

      st.copy(locations = st.locations ++ newLocs)
    }
  }

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))

  override def beginScope(): SX[Unit] =
    stack.beginScope(LocationsState[S]())

  override def endScope(): SX[Unit] = stack.endScope
}
