package aqua.semantics.header

import aqua.raw.RawContext
import aqua.semantics.SemanticError
import cats.{Comonad, Monoid}
import cats.data.*
import cats.syntax.monoid.*
import cats.data.Validated.validNec

case class HeaderSem[S[_], C](
  initCtx: C,
  finInitCtx: (C, C) => ValidatedNec[SemanticError[S], C]
) {

  def finCtx: C => ValidatedNec[SemanticError[S], C] =
    finInitCtx(_, initCtx)
}

object HeaderSem {

  implicit def headerSemMonoid[S[_]: Comonad](implicit
    rc: Monoid[RawContext]
  ): Monoid[HeaderSem[S, RawContext]] =
    new Monoid[HeaderSem[S, RawContext]] {
      override def empty: HeaderSem[S, RawContext] = HeaderSem(rc.empty, (c, _) => validNec(c))

      override def combine(
        a: HeaderSem[S, RawContext],
        b: HeaderSem[S, RawContext]
      ): HeaderSem[S, RawContext] =
        HeaderSem(
          a.initCtx |+| b.initCtx,
          (c, i) => a.finInitCtx(c, i).andThen(b.finInitCtx(_, i))
        )
    }
}
