package aqua.semantics.rules.report

import aqua.parser.lexer.Token

import cats.data.State

trait ReportErrors[S[_], X] extends ReportAlgebra[S, State[X, *]] {
  def apply(st: X, token: Token[S], hints: List[String]): X

  def report(token: Token[S], hints: List[String]): State[X, Unit] =
    State.modify(apply(_, token, hints))
}
