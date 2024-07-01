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

package aqua.semantics

import aqua.parser.Ast
import aqua.parser.lexer.Token

sealed trait SemanticError[S[_]]
case class RulesViolated[S[_]](token: Token[S], messages: List[String]) extends SemanticError[S]
case class HeaderError[S[_]](token: Token[S], message: String) extends SemanticError[S]
case class WrongAST[S[_]](ast: Ast[S]) extends SemanticError[S]
