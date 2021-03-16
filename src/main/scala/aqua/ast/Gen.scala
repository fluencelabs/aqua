package aqua.ast

import aqua.ast.algebra.types.ArrowType
import cats.Semigroup
import cats.free.Free

case class Gen(log: String, children: List[Gen] = Nil) {
  def lift[F[_]]: Free[F, Gen] = Free.pure(this)
}

object Gen {

  implicit object GenSemigroup extends Semigroup[Gen] {

    override def combine(x: Gen, y: Gen): Gen =
      x.copy(children = y :: x.children)
  }

  def noop = new Gen("noop")

  case class Arrow(`type`: ArrowType, gen: Gen)
}
