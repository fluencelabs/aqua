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

package aqua.semantics.rules.report

import aqua.parser.lexer.Token

import cats.data.State
import monocle.Lens

class ReportInterpreter[S[_], X](using
  lens: Lens[X, ReportState[S]]
) extends ReportAlgebra[S, State[X, *]] {

  override def error(token: Token[S], hints: List[String]): State[X, Unit] =
    State.modify(
      lens.modify(
        _.reportError(token, hints)
      )
    )

  override def warning(token: Token[S], hints: List[String]): State[X, Unit] =
    State.modify(
      lens.modify(
        _.reportWarning(token, hints)
      )
    )
}
