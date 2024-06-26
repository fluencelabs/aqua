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

package aqua.semantics.rules.definitions

import aqua.parser.lexer.{Name, NamedTypeToken, Token}
import aqua.semantics.rules.report.ReportAlgebra
import aqua.types.{ArrowType, Type}

import cats.data.{NonEmptyList, State}
import cats.syntax.functor.*
import cats.syntax.option.*
import monocle.Lens

class DefinitionsInterpreter[S[_], X](implicit
  lens: Lens[X, DefinitionsState[S]],
  report: ReportAlgebra[S, State[X, *]]
) extends DefinitionsAlgebra[S, State[X, *]] {
  type SX[A] = State[X, A]

  private def getState = State.get.map(lens.get)

  private def modify(f: DefinitionsState[S] => DefinitionsState[S]): SX[Unit] =
    State.modify(lens.modify(f))

  def define(name: Name[S], `type`: Type, defName: String): SX[Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case None =>
        modify(st =>
          st.copy(definitions =
            st.definitions.updated(
              name.value,
              DefinitionsState.Def(name, `type`)
            )
          )
        )
          .as(true)
      case Some(_) =>
        report
          .error(name, s"Cannot define $defName `${name.value}`, it was already defined above")
          .as(false)
    }

  override def defineDef(name: Name[S], `type`: Type): SX[Boolean] =
    define(name, `type`, "field")

  override def defineArrow(arrow: Name[S], `type`: ArrowType): SX[Boolean] =
    define(arrow, `type`, "arrow")

  override def purgeDefs(): SX[Map[String, DefinitionsState.Def[S]]] =
    getState.map(_.definitions).flatMap { defs =>
      for {
        _ <- modify(_.copy(definitions = Map.empty))
      } yield defs
    }

  def purgeArrows(token: Token[S]): SX[Option[NonEmptyList[(Name[S], ArrowType)]]] =
    getState.map(_.definitions).flatMap { defs =>
      val arrows = defs.values.collect { case DefinitionsState.Def(name, t: ArrowType) =>
        name -> t
      }
      NonEmptyList.fromList(arrows.toList) match {
        case Some(arrs) =>
          modify { st =>
            st.copy(definitions = Map.empty)
          }.as(arrs.some)
        case None =>
          report
            .error(token, "Cannot purge arrows, no arrows provided")
            .as(none)
      }
    }
}
