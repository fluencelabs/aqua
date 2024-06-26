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

import aqua.parser.expr.func.ParSeqExpr
import aqua.raw.Raw
import aqua.raw.ops.*
import aqua.raw.ops.ForTag
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, CollectionType, StreamType}

import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*

class ParSeqSem[S[_]](val expr: ParSeqExpr[S]) extends AnyVal {

  def program[F[_]: Monad](using
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F],
    M: ManglerAlgebra[F]
  ): Prog[F, Raw] =
    Prog
      .around(
        (
          ForSem.beforeFor(expr.item, expr.iterable),
          OnSem.beforeOn(expr.peerId, expr.via)
        ).tupled,
        // Without type of ops specified
        // scala compiler fails to compile this
        (iterableVia, ops: Raw) => {
          val (iterableVM, viaVM) = iterableVia
          after(iterableVM, viaVM, ops)
        }
      )
      .namesScope(expr.token)
      .abilitiesScope(expr.token)

  private def after[F[_]: Monad](
    iterableVM: Option[ValueRaw],
    viaVM: List[ValueRaw],
    ops: Raw
  )(using
    V: ValuesAlgebra[S, F]
  ): F[Raw] =
    V.valueToRaw(expr.peerId).map((_, iterableVM, ops)).flatMap {
      case (Some(peerId), Some(vm), FuncOp(op)) =>
        val onTag = OnTag(
          peerId = peerId,
          via = Chain.fromSeq(viaVM),
          strategy = OnTag.ReturnStrategy.Relay.some
        )

        val (item, pair) = ForSem.itemOrPair(expr.item)

        /**
         * `parseq` => par (`never` as `last` in `fold`)
         * So that peer initiating `parseq` would not continue execution past it
         */
        ForTag
          .par(item, vm, pair)
          .wrap(
            ParTag.wrap(
              onTag.wrap(op),
              NextTag(item).leaf
            )
          )
          .toFuncOp
          .pure
      case (None, _, _) => Raw.error("ParSeqSem: could not resolve `peerId`").pure
      case (_, None, _) => Raw.error("ParSeqSem: could not resolve `iterable`").pure
      case (_, _, _) => Raw.error("ParSeqSem: wrong body of `parseq` block").pure
    }
}
