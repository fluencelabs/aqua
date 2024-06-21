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

package aqua.lsp

import aqua.helpers.ext.Extension
import aqua.parser.lexer.LiteralToken
import aqua.parser.lift.FileSpan

// String literal from 'import' or 'use' with full path to imported file
case class TokenImportPath[S[_]](token: LiteralToken[S], path: String)

object TokenImportPath {
  def importPathsFromContext[S[_]](lspContext: LspContext[S]): List[TokenImportPath[S]] = {
    val importTokens = lspContext.importTokens
    val importPaths = lspContext.importPaths
    importTokens.map { lt =>
      val str = lt.value
      val unquoted = Extension.add(str.substring(1, str.length - 1))
      val path = importPaths.getOrElse(unquoted, unquoted)
      TokenImportPath(lt, path)
    }
  }
}
