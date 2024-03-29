package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain

// Parent == Par
object ParGroupBranch extends Ends with After {
  override def toString: String = "<par>/*"

  override def forceExit(current: Topology): Eval[ExitStrategy] =
    current.cursor
      .exportsUsedLaterFilter(
        _.op match {
          // This feels like a hack:
          // We suppose that `on` with Relay strategy
          // does not want to generate return transitions
          // because of it's exports.
          // This is used for `parseq` implementation.
          // We could not use `forceExit` of childs here
          // because it would cause infinite recursion.
          case OnModel(_, _, Some(OnModel.ReturnStrategy.Relay)) => false
          case _ => true
        }
      )
      .map(used =>
        if (used) ExitStrategy.Full
        else ExitStrategy.Empty
      )

  override def afterOn(current: Topology): Eval[TopologyPath] =
    afterParent(current)

  override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    pathAfterAndPingNext(current)

  override def endsOn(current: Topology): Eval[TopologyPath] = current.beforeOn
}
