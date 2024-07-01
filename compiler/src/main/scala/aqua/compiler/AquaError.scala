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

package aqua.compiler

import aqua.parser
import aqua.parser.lexer.Token
import aqua.semantics

import cats.data.NonEmptyChain

enum AquaError[+I, +E, S[_]] {
  case SourcesError(err: E)
  case ParserError(err: parser.ParserError[S])

  case ResolveImportsError(fromFile: I, token: Token[S], err: E)
  case ImportError(token: Token[S])
  case CycleError(modules: NonEmptyChain[I])

  case CompileError(err: semantics.SemanticError[S])
  case OutputError(compiled: AquaCompiled[I], err: E)
  case AirValidationError(errors: NonEmptyChain[String])
}
