package aqua.semantics.header

import aqua.semantics.SemanticError
import cats.data.*

case class HeaderSem[S[_], C](
  initCtx: C,
  finInitCtx: (C, C) => ValidatedNec[SemanticError[S], C]
) {

  def finCtx: C => ValidatedNec[SemanticError[S], C] =
    finInitCtx(_, initCtx)
}
