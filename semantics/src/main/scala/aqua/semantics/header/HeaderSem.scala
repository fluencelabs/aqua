package aqua.semantics.header

import aqua.raw.RawContext
import aqua.semantics.SemanticError

import cats.data.*
import cats.data.Validated.validNec
import cats.syntax.monoid.*
import cats.{Comonad, Monoid}

case class HeaderSem[S[_], C](
  initCtx: C,
  finInitCtx: (C, C) => ValidatedNec[SemanticError[S], C]
) {

  def finCtx: C => ValidatedNec[SemanticError[S], C] =
    finInitCtx(_, initCtx)
}

object HeaderSem {

  def fromInit[S[_], C](init: C): HeaderSem[S, C] =
    HeaderSem(init, (c, _) => validNec(c))

  given [S[_]: Comonad, C](using
    rc: Monoid[C]
  ): Monoid[HeaderSem[S, C]] with {
    override def empty: HeaderSem[S, C] = HeaderSem.fromInit(rc.empty)

    override def combine(
      a: HeaderSem[S, C],
      b: HeaderSem[S, C]
    ): HeaderSem[S, C] =
      HeaderSem(
        a.initCtx |+| b.initCtx,
        (c, i) => a.finInitCtx(c, i).andThen(b.finInitCtx(_, i))
      )
  }
}
