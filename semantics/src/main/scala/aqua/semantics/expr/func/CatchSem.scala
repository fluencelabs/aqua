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

import aqua.parser.expr.func.CatchExpr
import aqua.raw.Raw
import aqua.raw.ops.{AssignmentTag, FuncOp, SeqTag, TryTag}
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class CatchSem[S[_]](val expr: CatchExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        N.define(expr.name, ValueRaw.errorType),
        (_, g: Raw) =>
          g match {
            case FuncOp(op) =>
              TryTag.Catch
                .wrap(
                  SeqTag.wrap(
                    AssignmentTag(ValueRaw.error, expr.name.value).leaf,
                    op
                  )
                )
                .toFuncOp
                .pure
            case _ =>
              Raw.error("Wrong body of the `catch` expression").pure
          }
      )
      .abilitiesScope[S](expr.token)
      .namesScope(expr.token)
}
