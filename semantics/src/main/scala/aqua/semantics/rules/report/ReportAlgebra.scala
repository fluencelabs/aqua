package aqua.semantics.rules.report

import aqua.parser.lexer.Token

trait ReportAlgebra[S[_], Alg[_]] {
  def report(token: Token[S], hints: List[String]): Alg[Unit]

  def report(token: Token[S], hint: String): Alg[Unit] =
    report(token, hint :: Nil)
}
