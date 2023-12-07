package aqua.semantics

import aqua.parser.lexer.Token
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import scala.language.implicitConversions

sealed abstract class Prog[Alg[_]: Monad, A] extends (Alg[A] => Alg[A]) {
  self =>

  def wrap[R](before: Alg[R], after: (R, A) => Alg[A]): Prog[Alg, A] =
    new Prog[Alg, A] {

      override def apply(v1: Alg[A]): Alg[A] =
        before >>= (r => self(v1) >>= (a => after(r, a)))
    }

  def wrap[R](ar: RunAround[Alg, R, A]): Prog[Alg, A] =
    wrap(ar.before, ar.after)

  def abilitiesScope[S[_]](token: Token[S])(implicit Ab: AbilitiesAlgebra[S, Alg]): Prog[Alg, A] =
    wrap(
      RunAround(
        Ab.beginScope(token),
        (_: Unit, m: A) => Ab.endScope() as m
      )
    )

  def namesScope[S[_]](token: Token[S])(implicit N: NamesAlgebra[S, Alg]): Prog[Alg, A] =
    wrap(
      RunAround(
        N.beginScope(token),
        (_: Unit, m: A) => N.endScope() as m
      )
    )
}

case class RunAfter[Alg[_]: Monad, A](prog: Alg[A]) extends Prog[Alg, A] {

  override def apply(v1: Alg[A]): Alg[A] =
    v1 >> prog

}

case class RunAround[Alg[_]: Monad, R, A](before: Alg[R], after: (R, A) => Alg[A])
    extends Prog[Alg, A] {

  override def apply(v1: Alg[A]): Alg[A] =
    before >>= (r => v1 >>= (a => after(r, a)))
}

object Prog {

  implicit def leaf[Alg[_]: Monad, A](prog: Alg[A]): Prog[Alg, A] =
    RunAfter(prog)

  def after[Alg[_]: Monad, A](prog: A => Alg[A]): Prog[Alg, A] =
    RunAround(Monad[Alg].unit, (_: Unit, a: A) => prog(a))

  def after_[Alg[_]: Monad, A](prog: => Alg[A]): Prog[Alg, A] =
    after(_ => prog)

  def around[Alg[_]: Monad, R, A](before: Alg[R], after: (R, A) => Alg[A]): Prog[Alg, A] =
    RunAround(before, after)

  def noop[Alg[_]: Monad, A]: Prog[Alg, A] =
    RunAround(Monad[Alg].unit, (_: Unit, a: A) => Monad[Alg].pure(a))
}
