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

import aqua.raw.ops.{FuncOp, ParTag}
import aqua.parser.expr.func.ParExpr
import aqua.raw.Raw
import aqua.semantics.Prog
import cats.Monad
import cats.syntax.applicative.*

class ParSem[S[_]](val expr: ParExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Raw] =
    Prog.after[Alg, Raw] {
      case FuncOp(g) =>
        ParTag.Par.wrap(g).toFuncOp.pure[Alg]
      case g => g.pure[Alg]
    }
}
