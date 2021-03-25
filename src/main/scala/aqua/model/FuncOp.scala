package aqua.model

import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.kernel.Semigroup
import cats.syntax.semigroup._

case class FuncOp(body: Cofree[Chain, OpTag]) extends Model

object FuncOp {

  implicit object FuncOpSemigroup extends Semigroup[FuncOp] {
    override def combine(x: FuncOp, y: FuncOp): FuncOp = ???
  }

  def leaf(tag: OpTag): FuncOp = FuncOp(Cofree[Chain, OpTag](tag, Eval.now(Chain.empty)))

  def wrap(tag: OpTag, child: FuncOp): FuncOp = node(tag, Chain.one(child))
  def node(tag: OpTag, children: Chain[FuncOp]): FuncOp = ???
}
