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

import aqua.raw.{ErroredPart, Raw}
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.parser.expr.FuncExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import cats.{Applicative, Monad}
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class FuncSem[S[_]](val expr: FuncExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after {
      case arrow: ArrowRaw =>
        N.defineArrow(expr.name, arrow.`type`, isRoot = true) as FuncRaw(expr.name.value, arrow)

      case _ =>
        ErroredPart(expr.name.value).pure[Alg]
    }

}
