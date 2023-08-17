package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.OnModel

import cats.Eval

trait Before {

  def beforeOn(current: Topology): Eval[TopologyPath] =
    // Go to the parent, see where it begins
    current.parent.map(_.beginsOn) getOrElse
      // This means, we have no parent; then we're where we should be
      current.pathOn

}
