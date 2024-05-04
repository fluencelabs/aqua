package aqua.semantics.rules.locations

import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.{Monoid, Semigroup}

case class Variables[S[_]](
  variables: Map[String, List[VariableInfo[S]]] = Map.empty[String, List[VariableInfo[S]]]
) {

  def renameDefinitions(f: String => String): Variables[S] =
    copy(variables = variables.map { case (k, v) =>
      val newName = f(k)
      newName -> v.map(vi => vi.copy(definition = vi.definition.copy(name = newName)))
    })

  lazy val allLocations: List[TokenLocation[S]] =
    variables.values.flatMap(_.flatMap(_.allLocations)).toList

  def addDefinitions(newDefinitions: List[DefinitionInfo[S]]): Variables[S] = {
    copy(variables = newDefinitions.foldLeft(variables) { case (m, definition) =>
      m.updatedWith(definition.name) {
        case Some(v) => Some(VariableInfo(definition) +: v)
        case None => Some(VariableInfo(definition) :: Nil)
      }
    })
  }

  def updateFirst(
    name: String,
    token: Token[S]
  ): Variables[S] = {
    copy(variables = variables.updatedWith(name) {
      case Some(vars) =>
        Some(
          vars.updateFirst(
            _.definition.name == name,
            v => v.copy(occurrences = token +: v.occurrences)
          )
        )
      case None => None
    })
  }
}

object Variables {

  given [S[_]]: Semigroup[Variables[S]] with {

    override def combine(x: Variables[S], y: Variables[S]): Variables[S] = {
      Variables(y.variables.foldLeft(x.variables) { case (m, (k, v)) =>
        m.updatedWith(k) {
          case None => Some(v)
          case Some(vars) => Some((vars ++ v).distinct)
        }
      })
    }
  }
}
