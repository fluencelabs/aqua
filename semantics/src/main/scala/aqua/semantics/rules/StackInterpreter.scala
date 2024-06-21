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

package aqua.semantics.rules

import aqua.parser.lexer.Token

import cats.data.State
import cats.syntax.functor.*
import cats.syntax.applicative.*
import monocle.Lens

case class StackInterpreter[S[_], X, St, Fr](
  stackLens: Lens[St, List[Fr]]
)(using lens: Lens[X, St]) {
  type SX[A] = State[X, A]

  def getState: SX[St] = State.get.map(lens.get)
  def setState(st: St): SX[Unit] = State.modify(s => lens.replace(st)(s))

  def modify(f: St => St): SX[Unit] =
    State.modify(lens.modify(f))

  def mapStackHead[A](ifStackEmpty: A)(f: Fr => (Fr, A)): SX[A] =
    mapStackHeadM(ifStackEmpty.pure)(f.andThen(_.pure))

  def mapStackHead_(f: Fr => Fr): SX[Unit] =
    mapStackHead(())(f.andThen(_ -> ()))

  def mapStackHeadM[A](ifStackEmpty: => SX[A])(f: Fr => SX[(Fr, A)]): SX[A] =
    getState.map(stackLens.get).flatMap {
      case head :: tail =>
        f(head).flatMap { case (updated, result) =>
          modify(stackLens.replace(updated :: tail)).as(result)
        }
      case Nil => ifStackEmpty
    }

  def endScope: SX[Unit] =
    modify(stackLens.modify(_.tail))

  def beginScope(emptyFrame: Fr): SX[Unit] =
    modify(stackLens.modify(emptyFrame :: _))
}
