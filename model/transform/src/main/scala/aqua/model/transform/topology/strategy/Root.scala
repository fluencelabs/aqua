package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.OnModel

import cats.Eval

object Root extends Before with Ends with After {
  override def toString: String = "<root>"

  override def beforeOn(current: Topology): Eval[TopologyPath] = current.beginsOn

  override def endsOn(current: Topology): Eval[TopologyPath] = current.pathOn

  override def afterOn(current: Topology): Eval[TopologyPath] = current.pathOn

  override def forceExit(current: Topology): Eval[ExitStrategy] = Eval.now(ExitStrategy.Empty)
}
