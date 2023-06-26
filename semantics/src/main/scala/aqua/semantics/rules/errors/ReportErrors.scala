package aqua.semantics.rules.errors

import aqua.parser.lexer.Token

import cats.data.State

trait ReportErrors[S[_], X] extends ErrorsAlgebra[S, State[X, *]] {
  def apply(st: X, token: Token[S], hints: List[String]): X

  def report(token: Token[S], hints: List[String]): State[X, Unit] =
    State.modify(apply(_, token, hints))
}
