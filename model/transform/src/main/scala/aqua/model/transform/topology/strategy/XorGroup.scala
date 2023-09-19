package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.OnModel

import cats.Eval
import cats.data.Chain

object XorGroup extends Ends {
  override def toString: String = "<xor>"

  // Xor tag ends where any child ends; can't get first one as it may lead to recursion
  override def endsOn(current: Topology): Eval[TopologyPath] =
    firstChildFinally(current)

}
