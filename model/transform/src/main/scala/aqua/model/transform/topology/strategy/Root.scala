package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.OnModel

import cats.Eval

object Root extends Before with Ends with After {
  override def toString: String = "<root>"

  override def beforeOn(current: Topology): Eval[List[OnModel]] = current.beginsOn

  override def endsOn(current: Topology): Eval[List[OnModel]] = current.pathOn

  override def afterOn(current: Topology): Eval[List[OnModel]] = current.pathOn

  override def forceExit(current: Topology): Eval[Boolean] = Eval.now(false)
}
