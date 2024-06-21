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

import aqua.parser.expr.func.OnExpr
import aqua.parser.lexer.ValueToken
import aqua.raw.Raw
import aqua.raw.ops.{FuncOp, OnTag}
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{CollectionType, OptionType, ScalarType}

import cats.data.Chain
import cats.data.OptionT
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Monad, Traverse}

class OnSem[S[_]](val expr: OnExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        OnSem.beforeOn(expr.peerId, expr.via),
        (viaVM: List[ValueRaw], ops: Raw) =>
          ops match {
            case FuncOp(op) =>
              V.valueToRaw(expr.peerId).map {
                case Some(om) =>
                  OnTag(
                    om,
                    Chain.fromSeq(viaVM)
                  ).wrap(op).toFuncOp
                case _ =>
                  Raw.error("OnSem: Impossible error")
              }

            case m => Raw.error("On body is not an op, it's " + m).pure[Alg]
          }
      )
      .abilitiesScope(expr.peerId)
}

object OnSem {

  def beforeOn[S[_], Alg[_]: Monad](
    peerId: ValueToken[S],
    via: List[ValueToken[S]]
  )(using
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Alg[List[ValueRaw]] =
    // TODO: Remove ensureIsString, use valueToStringRaw
    V.ensureIsString(peerId) *> via
      .traverse(v =>
        OptionT(V.valueToRaw(v)).filterF { vm =>
          val expectedType = vm.`type` match {
            case _: CollectionType => OptionType(ScalarType.string)
            case _ => ScalarType.string
          }

          T.ensureTypeMatches(v, expectedType, vm.`type`)
        }
      )
      .getOrElse(List.empty)
}
