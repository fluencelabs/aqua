package aqua.semantics.rules.report

import aqua.parser.lexer.Token

import cats.data.State
import monocle.Lens

class ReportInterpreter[S[_], X](using
  lens: Lens[X, ReportState[S]]
) extends ReportAlgebra[S, State[X, *]] {

  override def error(token: Token[S], hints: List[String]): State[X, Unit] =
    State.modify(
      lens.modify(
        _.reportError(token, hints)
      )
    )

  override def warning(token: Token[S], hints: List[String]): State[X, Unit] =
    State.modify(
      lens.modify(
        _.reportWarning(token, hints)
      )
    )
}
