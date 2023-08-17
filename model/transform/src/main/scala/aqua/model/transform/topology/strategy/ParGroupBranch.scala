package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain

// Parent == Par
object ParGroupBranch extends Ends with After {
  override def toString: String = "<par>/*"

  override def forceExit(current: Topology): Eval[ExitStrategy] =
    Eval.later {
      if (current.cursor.exportsUsedLater) ExitStrategy.Full
      else ExitStrategy.Empty
    }

  override def afterOn(current: Topology): Eval[List[OnModel]] =
    afterParent(current)

  override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    pathAfterAndPingNext(current)

  override def endsOn(current: Topology): Eval[List[OnModel]] = current.beforeOn
}
