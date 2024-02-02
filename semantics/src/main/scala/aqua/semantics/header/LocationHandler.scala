package aqua.semantics.header

import aqua.parser.lexer.Token
import aqua.raw.RawContext

trait LocationHandler[S[_], A] {
  def addOccurences(ctx: A, tokens: List[(String, Token[S])], isRoot: Boolean): A
}

class LocationHandlerRaw[S[_]] extends LocationHandler[S, RawContext] {

  override def addOccurences(ctx: RawContext, tokens: List[(String, Token[S])], isRoot: Boolean): RawContext = ctx
}
