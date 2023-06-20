package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.OnModel

import cats.Eval

object SeqNext extends Begins {
  override def toString: String = "<seq>/<next>"

  override def beginsOn(current: Topology): Eval[List[OnModel]] =
    current.parents.find(_.isForModel).map(_.beginsOn) getOrElse super.beginsOn(current)
}
