package aqua.semantics.rules.errors

import aqua.parser.lexer.Token

trait ErrorsAlgebra[S[_], Alg[_]] {
  def report(token: Token[S], hints: List[String]): Alg[Unit]
}
