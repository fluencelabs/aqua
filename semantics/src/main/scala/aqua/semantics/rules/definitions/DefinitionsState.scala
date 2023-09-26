package aqua.semantics.rules.definitions

import aqua.parser.lexer.{Name, Token}
import aqua.types.Type

import DefinitionsState.Def

case class DefinitionsState[S[_]](
  definitions: Map[String, Def[S]] = Map.empty[String, Def[S]]
)

object DefinitionsState {

  final case class Def[S[_]](
    name: Name[S],
    `type`: Type
  )
}
