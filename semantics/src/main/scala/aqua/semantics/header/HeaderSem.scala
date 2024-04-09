package aqua.semantics.header

import aqua.raw.RawContext
import aqua.semantics.SemanticError

import cats.data.*
import cats.data.Validated.validNec
import cats.syntax.monoid.*
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

/**
 * Semantics for handling a header expression
 * (e.g. `aqua Name declares *`, `export`, `use` etc.)
 *
 * @param init Initial context that will be combined with others and passed to body semantics
 * @param fin Finalization function to which context after body semantics will be passed
 */
case class HeaderSem[S[_], C](
  init: C,
  fin: C => ValidatedNec[SemanticError[S], C]
)

object HeaderSem {

  def fromInit[S[_], C](init: C): HeaderSem[S, C] =
    HeaderSem(init, c => c.validNec)

  given [S[_]: Comonad, C](using
    rc: Monoid[C]
  ): Monoid[HeaderSem[S, C]] with {

    override def empty: HeaderSem[S, C] =
      HeaderSem.fromInit(rc.empty)

    override def combine(
      a: HeaderSem[S, C],
      b: HeaderSem[S, C]
    ): HeaderSem[S, C] =
      HeaderSem(
        a.init |+| b.init,
        c => a.fin(c) |+| b.fin(c)
      )
  }
}
