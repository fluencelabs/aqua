package aqua.semantics

import aqua.parser.lexer.Token
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

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

  def abilitiesScope[F[_]](token: Token[F])(implicit Ab: AbilitiesAlgebra[F, Alg]): Prog[Alg, A] =
    wrap(
      RunAround(
        Ab.beginScope(token),
        (_: Unit, m: A) => Ab.endScope() as m
      )
    )
}

case class RunAfter[Alg[_]: Monad, A](prog: Alg[A]) extends Prog[Alg, A] {

  override def apply(v1: Alg[A]): Alg[A] =
    v1 >> prog

}

case class RunAround[Alg[_]: Monad, R, A](before: Alg[R], after: (R, A) => Alg[A]) extends Prog[Alg, A] {

  override def apply(v1: Alg[A]): Alg[A] =
    before >>= (r => v1 >>= (a => after(r, a)))
}

object Prog {

  implicit def leaf[Alg[_]: Monad, A](prog: Alg[A]): Prog[Alg, A] =
    RunAfter(prog)

  def after[Alg[_]: Monad, A](prog: A => Alg[A]): Prog[Alg, A] =
    RunAround(Monad[Alg].unit, (_: Unit, a: A) => prog(a))

  def around[Alg[_]: Monad, R, A](before: Alg[R], after: (R, A) => Alg[A]): Prog[Alg, A] =
    RunAround(before, after)

  def noop[Alg[_]: Monad, A]: Prog[Alg, A] =
    RunAround(Monad[Alg].unit, (_: Unit, a: A) => Monad[Alg].pure(a))
}
