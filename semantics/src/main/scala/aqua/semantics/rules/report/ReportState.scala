package aqua.semantics.rules.report

import aqua.semantics.{RulesViolated, SemanticError}
import aqua.parser.lexer.Token

import cats.data.Chain
import cats.kernel.Monoid

final case class ReportState[S[_]](
  errors: Chain[SemanticError[S]] = Chain.empty[SemanticError[S]]
) {

  def reportError(token: Token[S], hints: List[String]): ReportState[S] =
    copy(errors = errors.append(RulesViolated(token, hints)))
}

object ReportState {

  given [S[_]]: Monoid[ReportState[S]] with {
    override val empty: ReportState[S] = ReportState()

    override def combine(x: ReportState[S], y: ReportState[S]): ReportState[S] =
      ReportState(errors = x.errors ++ y.errors)
  }
}