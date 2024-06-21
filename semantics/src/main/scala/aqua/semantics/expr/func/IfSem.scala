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

import aqua.parser.expr.func.IfExpr
import aqua.raw.Raw
import aqua.raw.ops.{FuncOp, IfTag}
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.ScalarType
import aqua.types.Type

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class IfSem[S[_]](val expr: IfExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        V.valueToRaw(expr.value)
          .flatMap(
            _.flatTraverse(raw =>
              T.ensureTypeMatches(
                token = expr.value,
                expected = ScalarType.bool,
                givenType = raw.`type`
              ).map(Option.when(_)(raw))
            )
          ),
        // Without type of ops specified
        // scala compiler fails to compile this
        (value, ops: Raw) =>
          (value, ops) match {
            case (Some(vr), FuncOp(op)) =>
              IfTag(vr).wrap(op).toFuncOp.pure
            case (None, _) => Raw.error("`if` expression errored in matching types").pure
            case _ => Raw.error("Wrong body of the `if` expression").pure
          }
      )
      .abilitiesScope[S](expr.token)
      .namesScope[S](expr.token)
}
