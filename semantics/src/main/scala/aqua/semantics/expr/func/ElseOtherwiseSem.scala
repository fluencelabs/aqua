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

import aqua.parser.expr.func.ElseOtherwiseExpr
import aqua.raw.Raw
import aqua.raw.ops.{FuncOp, IfTag, TryTag}
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.functor.*

class ElseOtherwiseSem[S[_]](val expr: ElseOtherwiseExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .after((ops: Raw) =>
        ops match {
          case FuncOp(op) =>
            expr.kind
              .fold(
                ifElse = IfTag.Else,
                ifOtherwise = TryTag.Otherwise
              )
              .wrap(op)
              .toFuncOp
              .pure
          case _ =>
            val name = expr.kind.fold("`else`", "`otherwise`")
            Raw.error(s"Wrong body of the $name expression").pure
        }
      )
      .abilitiesScope(expr.token)
      .namesScope(expr.token)
}
