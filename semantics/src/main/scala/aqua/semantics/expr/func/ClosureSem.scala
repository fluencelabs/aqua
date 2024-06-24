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

package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.raw.ops.ClosureTag
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra

import cats.Applicative
import cats.data.Chain
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.Monad

class ClosureSem[S[_]](val expr: ClosureExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after {
      case arrow: ArrowRaw =>
        // TODO: if detached, clear all locally-defined abilities
        N.defineArrow(
          expr.name,
          arrow.`type`,
          isRoot = false
        ) as ClosureTag(FuncRaw(expr.name.value, arrow), expr.detach.isDefined).funcOpLeaf

      case _ =>
        Raw.error("Closure must continue with an arrow definition").pure[Alg]
    }

}
