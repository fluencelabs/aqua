package aqua.lsp

import aqua.parser.lexer.Token
import aqua.semantics.lsp.{TokenInfo, TokenType, TokenTypeInfo}
import aqua.semantics.rules.{ReportError, StackInterpreter}
import aqua.semantics.rules.locations.{LocationsAlgebra, LocationsState}
import cats.data.State
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging

class LocationsInterpreter[S[_], X](implicit
  lens: Lens[X, LocationsState[S]],
  error: ReportError[S, X]
) extends LocationsAlgebra[S, State[X, *]] with Logging {

  type SX[A] = State[X, A]

  val stack = new StackInterpreter[S, X, LocationsState[S], LocationsState[S]](
    GenLens[LocationsState[S]](_.stack)
  )

  import stack.{getState, mapStackHead, modify, report}

  override def addToken(name: String, token: Token[S]): State[X, Unit] = modify { st =>
    println(s"add token $name")
    st.copy(tokens = st.tokens.updated(name, token))
  }

  private def combineFieldName(name: String, field: String): String = name + "." + field

  override def addTokenWithFields(
    name: String,
    token: Token[S],
    fields: List[(String, Token[S])]
  ): State[X, Unit] = modify { st =>
    println(s"add token for: $name with fields ${fields.map(_._1)}")
    st.copy(tokens =
      st.tokens ++ ((name, token) +: fields.map(kv => (combineFieldName(name, kv._1), kv._2))).toMap
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
      println(s"add location for $name")

      val newLoc: Option[Token[S]] = st.stack.collectFirst {
        case frame if frame.tokens.contains(name) => frame.tokens(name)
      } orElse st.tokens.get(name)
      println(s"new loc exists: " + newLoc.nonEmpty)
      println(s"all tokens in interpreter: " + st.tokens)
      st.copy(locations = st.locations ++ newLoc.map(token -> _).toList)
    }
  }

  def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] = {
    modify { st =>

      val newLocs = locations.flatMap { case (name, token) =>
        (st.stack.collectFirst {
          case frame if frame.tokens.contains(name) => frame.tokens(name)
        } orElse st.tokens.get(name)).map(token -> _)
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
