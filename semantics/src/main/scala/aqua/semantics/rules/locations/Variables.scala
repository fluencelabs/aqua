package aqua.semantics.rules.locations

import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.{Monoid, Semigroup}
import cats.syntax.align.*

case class Variables[S[_]](
  variables: Map[String, List[VariableInfo[S]]] = Map.empty[String, List[VariableInfo[S]]]
) {

  def renameDefinitions(f: PartialFunction[String, String]): Variables[S] =
    copy(variables = variables.map { case (k, v) =>
      f.andThen { newName =>
        newName -> v.map(vi => vi.copy(definition = vi.definition.copy(name = newName)))
      }.orElse { _ =>
        k -> v
      }(k)
    })

  lazy val allLocations: List[TokenLocation[S]] =
    variables.values.flatMap(_.flatMap(_.allLocations)).toList

  lazy val definitions: List[DefinitionInfo[S]] =
    variables.values.flatMap(_.map(_.definition)).toList

  def addDefinitions(newDefinitions: List[DefinitionInfo[S]]): Variables[S] = {
    copy(variables =
      newDefinitions
        .map(d => d.name -> List(VariableInfo(d)))
        .toMap
        .alignCombine(variables)
    )
  }

  /**
   * Add occurrance by name to the first (last added) definition.
   */
  def addOccurence(
    name: String,
    token: Token[S]
  ): Variables[S] = {
    copy(variables =
      variables.updatedWith(name)(
        _.map(
          _.updateFirst(
            _.definition.name == name,
            v => v.copy(occurrences = token +: v.occurrences)
          )
        )
      )
    )
  }
}

object Variables {

  given [S[_]]: Semigroup[Variables[S]] with {

    override def combine(x: Variables[S], y: Variables[S]): Variables[S] =
      Variables(x.variables.alignCombine(y.variables).view.mapValues(_.distinct).toMap)
  }
}
