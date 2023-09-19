package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.OnModel

import cats.Eval

object SeqGroup extends Ends {
  override def toString: String = "<seq>"

  override def endsOn(current: Topology): Eval[TopologyPath] =
    lastChildFinally(current)
}
