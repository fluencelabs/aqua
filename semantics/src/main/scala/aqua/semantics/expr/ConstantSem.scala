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

package aqua.semantics.expr

import aqua.parser.expr.ConstantExpr
import aqua.raw.{ConstantRaw, ErroredPart, Raw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.Monad

class ConstantSem[S[_]](val expr: ConstantExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = {
    for {
      defined <- N.constantDefined(expr.name)
      v <- V.valueToRaw(expr.value)
      model <- (defined, v.map(v => v -> v.`type`), expr.skipIfAlreadyDefined) match {
        case (Some(definedType), Some((vm, actualType)), true) =>
          T.ensureTypeMatches(expr.value, definedType, actualType).map {
            case true =>
              Raw.empty(s"Constant with name ${expr.name} was already defined, skipping")
            case false =>
              ErroredPart(expr.name.value)
          }
        case (Some(_), _, _) =>
          Raw.error(s"Name '${expr.name.value}' was already defined").pure[Alg]
        case (_, None, _) =>
          ErroredPart(expr.name.value).pure[Alg]
        case (_, Some(t), _) =>
          N.defineConstant(expr.name, t._2) as (ConstantRaw(
            expr.name.value,
            t._1,
            expr.skipIfAlreadyDefined
          ): Raw)
      }
    } yield model
  }
}
