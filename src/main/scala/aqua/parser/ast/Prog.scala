package aqua.parser.ast

import cats.free.Free
import cats.syntax.flatMap._

import scala.language.implicitConversions

sealed trait Prog[Alg[_], A] extends (Free[Alg, A] => Free[Alg, A])

case class RunAfter[Alg[_], A](prog: Free[Alg, A]) extends Prog[Alg, A] {

  override def apply(v1: Free[Alg, A]): Free[Alg, A] =
    v1 >> prog

}

case class RunAround[Alg[_], R, A](before: Free[Alg, R], after: R => Free[Alg, A]) extends Prog[Alg, A] {

  override def apply(v1: Free[Alg, A]): Free[Alg, A] =
    before >>= (a => v1 >> after(a))
}

object Prog {

  implicit def leaf[Alg[_], A](prog: Free[Alg, A]): Prog[Alg, A] =
    RunAfter(prog)

  def after[Alg[_], A](prog: Free[Alg, A]): Prog[Alg, A] =
    RunAfter(prog)

  def around[Alg[_], R, A](before: Free[Alg, R], after: R => Free[Alg, A]): Prog[Alg, A] =
    RunAround(before, after)

}
