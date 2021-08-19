package aqua.semantics.header

import aqua.model.AquaContext
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.{Comonad, Monoid}
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import aqua.parser.Ast
import aqua.semantics.SemanticError

case class HeaderSem(initCtx: AquaContext) {
  def finalize(ctx: AquaContext): ValidatedNec[SemanticError[S], AquaContext] = validNec(ctx)
}

object HeaderSem {

  def sem[S[_]: Comonad](imports: Map[String, AquaContext], header: Ast.Head[S])(implicit
    acm: Monoid[AquaContext]
  ): ValidatedNec[SemanticError[S], HeaderSem] = {
    val initCtx =
      imports.values.foldLeft(Monoid[AquaContext].empty)(Monoid[AquaContext].combine)

    validNec(HeaderSem(initCtx))
  }

}
