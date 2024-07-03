/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.semantics.rules.locations

import aqua.helpers.data.PName
import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.{Monoid, Semigroup}
import cats.syntax.align.*
import scribe.Logging

case class Variables[S[_]](
  variables: Map[PName, List[VariableInfo[S]]] = Map.empty[PName, List[VariableInfo[S]]]
) extends Logging {

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
    copy(variables = {
      if (!variables.contains(name)) {
        logger.warn(s"Name `$name` not found")
      }

      variables.updatedWith(name)(
        _.map(
          _.updateFirst(
            _.isFor(name),
            _.addOccurence(token)
          )
        )
      )
    })
  }

  def debug: String =
    variables.flatMap { case (k, vis) =>
      s"$k:" :: vis.flatMap { vi =>
        s"-> ${vi.definition}:" :: vi.occurrences.map { oc =>
          s"--| $oc"
        }
      }
    }.mkString("\n")
}

object Variables {

  given [S[_]]: Semigroup[Variables[S]] with {

    override def combine(x: Variables[S], y: Variables[S]): Variables[S] =
      Variables(x.variables.alignCombine(y.variables).view.mapValues(_.distinct).toMap)
  }
}
