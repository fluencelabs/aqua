package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.OnModel

import cats.Eval

// Parent == Seq, On
object SeqGroupBranch extends Before with After {
  override def toString: String = "<seq>/*"

  // If parent is seq, then before this node we are where previous node, if any, ends
  override def beforeOn(current: Topology): Eval[List[OnModel]] =
    // Where we are after the previous node in the parent
    current.prevSibling
      .map(_.finallyOn) getOrElse super.beforeOn(current)

  override def afterOn(current: Topology): Eval[List[OnModel]] =
    current.nextSibling.map(_.beginsOn) getOrElse afterParent(current)

}
