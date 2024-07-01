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

import aqua.helpers.syntax.optiont.*
import aqua.parser.expr.func.ReturnExpr
import aqua.raw.Raw
import aqua.raw.ops.ReturnTag
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.Monad
import cats.data.{NonEmptyList, OptionT}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class ReturnSem[S[_]](val expr: ReturnExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = (
    for {
      vals <- expr.values.traverse(v =>
        OptionT(
          V.valueToRaw(v)
        ).map(v -> _)
      )
      _ <- OptionT.withFilterF(
        T.checkArrowReturn(vals)
      )
    } yield ReturnTag(vals.map(_._2)).leaf.toFuncOp
  ).getOrElse(
    Raw.error("Return values validation failed")
  )
}
