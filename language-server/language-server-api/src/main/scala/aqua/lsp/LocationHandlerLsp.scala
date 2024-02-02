package aqua.lsp

import aqua.parser.lexer.Token
import aqua.semantics.header.LocationHandler

class LocationHandlerLsp[S[_]] extends LocationHandler[S, LspContext[S]] {

  override def addOccurences(
    ctx: LspContext[S],
    tokens: List[(String, Token[S])],
    isRoot: Boolean
  ): LspContext[S] = {
    val variables = ctx.variables
    val variablesWithNewOccurences = tokens.foldLeft(variables) { case (vars, (n, t)) =>
      vars.map { vi =>
        if (vi.definition.name == n && vi.definition.isRoot == isRoot)
          vi.copy(occurrences = vi.occurrences :+ t)
        else
          vi
      }
    }
    ctx.copy(variables = variablesWithNewOccurences)
  }
}
