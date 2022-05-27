package aqua.semantics.lsp
import aqua.raw.RawContext

case class LspContext[S[_]](raw: RawContext, locations: Any /*[S]*/ ) {}
