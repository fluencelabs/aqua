package aqua.ast

import cats.free.Free

case class Gen(log: String) {
  def lift[F[_]]: Free[F, Gen] = Free.pure(this)
}

object Gen {
  def noop = new Gen("noop")
}
