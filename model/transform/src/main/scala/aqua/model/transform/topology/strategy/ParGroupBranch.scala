package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain

// Parent == Par
object ParGroupBranch extends Ends with After {
  override def toString: String = "<par>/*"

  override def forceExit(current: Topology): Eval[Boolean] =
    Eval.later(current.cursor.exportsUsedLater)

  override def afterOn(current: Topology): Eval[List[OnModel]] =
    afterParent(current)

  override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    pathAfterAndPingNext(current)

  override def endsOn(current: Topology): Eval[List[OnModel]] = current.beforeOn
}
