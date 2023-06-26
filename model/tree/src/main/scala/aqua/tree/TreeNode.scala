package aqua.tree

import cats.data.Chain
import cats.free.Cofree
import cats.Eval

trait TreeNode[T <: TreeNode[T]] {
  self: T =>
  type Tree = Cofree[Chain, T]

  lazy val leaf: Tree = Cofree(self, Eval.now(Chain.empty))

  def wrap(children: Tree*): Tree = wrap(Chain.fromSeq(children))

  def wrap(children: List[Tree]): Tree = wrap(Chain.fromSeq(children))

  def wrap(children: Chain[Tree]): Tree = Cofree(self, Eval.now(children))

  protected def wrapNonEmpty(children: List[Tree], empty: Tree): Tree = children match {
    case Nil => empty
    case x :: Nil => x
    case _ => Cofree(self, Eval.now(Chain.fromSeq(children)))
  }

}
