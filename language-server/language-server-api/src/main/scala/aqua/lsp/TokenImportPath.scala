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
