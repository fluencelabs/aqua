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

import aqua.parser.expr.AliasExpr
import aqua.raw.{ErroredPart, Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.Applicative
import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class AliasSem[S[_]](val expr: AliasExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using T: TypesAlgebra[S, Alg]): Prog[Alg, Raw] =
    T.resolveType(expr.target).flatMap {
      case Some(t) => T.defineAlias(expr.name, t) as (TypeRaw(expr.name.value, t): Raw)
      case None => Applicative[Alg].pure(ErroredPart(expr.name.value))
    }
}
