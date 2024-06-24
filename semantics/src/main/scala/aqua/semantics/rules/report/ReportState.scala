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
import aqua.semantics.{RulesViolated, SemanticError, SemanticWarning}

import cats.data.Chain
import cats.kernel.Monoid

final case class ReportState[S[_]](
  errors: Chain[SemanticError[S]] = Chain.empty[SemanticError[S]],
  warnings: Chain[SemanticWarning[S]] = Chain.empty[SemanticWarning[S]]
) {

  def reportError(token: Token[S], hints: List[String]): ReportState[S] =
    copy(errors = errors.append(RulesViolated(token, hints)))

  def reportWarning(token: Token[S], hints: List[String]): ReportState[S] =
    copy(warnings = warnings.append(SemanticWarning(token, hints)))
}
