package aqua.semantics.rules.locations

import aqua.helpers.data.PName
import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.{Monoid, Semigroup}
import cats.syntax.align.*

case class Variables[S[_]](
  variables: Map[PName, List[VariableInfo[S]]] = Map.empty[PName, List[VariableInfo[S]]]
) {

  def renameDefinitions(f: PartialFunction[PName, PName]): Variables[S] =
    copy(variables = variables.map { case (k, v) =>
      f.andThen { newName =>
        // Should we rename ALL variables? Why not only the first?
        newName -> v.map(_.rename(newName))
      }.lift(k).getOrElse(k -> v)
    })

  lazy val locations: List[TokenLocation[S]] =
    variables.values.flatMap(_.flatMap(_.locations)).toList

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
    name: PName,
    token: Token[S]
  ): Variables[S] = {
    copy(variables =
      variables.updatedWith(name)(
        _.map(
          _.updateFirst(
            _.isFor(name),
            _.addOccurence(token)
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
