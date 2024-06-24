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

import aqua.parser.expr.RootExpr
import aqua.raw.{Raw, RawContext, RawPart}
import aqua.semantics.Prog
import cats.syntax.applicative.*
import cats.Monad

class RootSem[S[_]](val expr: RootExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Raw] =
    Prog.after {
      case sm: RawPart =>
        sm.pure[Alg]
      case m =>
        RawPart
          .contextPart(m)
          // TODO .getOrElse(Model.error("Root contains not a script model, it's " + m))
          .pure[Alg]

    }
}
