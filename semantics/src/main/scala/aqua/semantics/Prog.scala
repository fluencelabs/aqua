package aqua.semantics

import aqua.parser.lexer.Token
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.language.implicitConversions

sealed trait Prog[Alg[_], A] extends (Free[Alg, A] => Free[Alg, A]) {
  self =>

  def wrap[R](before: Free[Alg, R], after: (R, A) => Free[Alg, A]): Prog[Alg, A] =
    new Prog[Alg, A] {

      override def apply(v1: Free[Alg, A]): Free[Alg, A] =
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

case class RunAfter[Alg[_], A](prog: Free[Alg, A]) extends Prog[Alg, A] {

  override def apply(v1: Free[Alg, A]): Free[Alg, A] =
    v1 >> prog

}

case class RunAround[Alg[_], R, A](before: Free[Alg, R], after: (R, A) => Free[Alg, A])
    extends Prog[Alg, A] {

  override def apply(v1: Free[Alg, A]): Free[Alg, A] =
    before >>= (r => v1 >>= (a => after(r, a)))
}

object Prog {

  implicit def leaf[Alg[_], A](prog: Free[Alg, A]): Prog[Alg, A] =
    RunAfter(prog)

  def after[Alg[_], A](prog: A => Free[Alg, A]): Prog[Alg, A] =
    RunAround(Free.pure(()), (_: Unit, a: A) => prog(a))

  def around[Alg[_], R, A](before: Free[Alg, R], after: (R, A) => Free[Alg, A]): Prog[Alg, A] =
    RunAround(before, after)

  def noop[Alg[_], A]: Prog[Alg, A] =
    RunAround(Free.pure(()), (_: Unit, a: A) => Free.pure(a))
}
