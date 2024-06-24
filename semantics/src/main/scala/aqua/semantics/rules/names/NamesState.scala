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

package aqua.semantics.rules.names

import aqua.parser.lexer.{Name, Token}
import aqua.raw.RawContext
import aqua.types.{ArrowType, Type}

import cats.kernel.Monoid
import cats.syntax.functor.*

case class NamesState[S[_]](
  stack: List[NamesState.Frame[S]] = Nil,
  rootArrows: Map[String, ArrowType] = Map.empty[String, ArrowType],
  constants: Map[String, Type] = Map.empty[String, Type],
  definitions: Map[String, Name[S]] = Map.empty[String, Name[S]]
) {

  def allNames: LazyList[String] =
    LazyList
      .from(stack)
      .flatMap(s => s.names.keys ++ s.arrows.keys)
      .appendedAll(rootArrows.keys)
      .appendedAll(constants.keys)

  def allArrows: LazyList[String] =
    LazyList.from(stack).flatMap(_.arrows.keys).appendedAll(rootArrows.keys)
}

object NamesState {

  case class Frame[S[_]](
    token: Token[S],
    names: Map[String, Type] = Map.empty[String, Type],
    derivedFrom: Map[String, Set[String]] = Map.empty,
    arrows: Map[String, ArrowType] = Map.empty[String, ArrowType]
  ) {

    def addName(n: Name[S], t: Type): NamesState.Frame[S] =
      copy[S](names = names.updated(n.value, t))

    def addInternalName(n: String, t: Type): NamesState.Frame[S] =
      copy[S](names = names.updated(n, t))

    def derived(n: Name[S], from: Set[String]): NamesState.Frame[S] =
      copy[S](derivedFrom =
        derivedFrom + (n.value -> from.flatMap(f => derivedFrom.get(f).fold(Set(f))(_ + f)))
      )

    def addArrow(n: Name[S], at: ArrowType): NamesState.Frame[S] =
      copy[S](arrows = arrows.updated(n.value, at))
  }

  def init[S[_]](context: RawContext): NamesState[S] =
    NamesState(
      rootArrows = context.allFuncs.map { case (s, fc) =>
        (s, fc.arrow.`type`)
      },
      constants = context.allValues.map { case (s, vm) => (s, vm.`type`) }
    )

}
