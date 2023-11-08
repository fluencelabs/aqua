package aqua.semantics.rules.report

import aqua.parser.lexer.Token

trait ReportAlgebra[S[_], Alg[_]] {
  def error(token: Token[S], hints: List[String]): Alg[Unit]

  def error(token: Token[S], hint: String): Alg[Unit] =
    error(token, hint :: Nil)
    
  def internalError(hint: String): Alg[Unit]
    

  def warning(token: Token[S], hints: List[String]): Alg[Unit]

  def warning(token: Token[S], hint: String): Alg[Unit] =
    warning(token, hint :: Nil)
}
