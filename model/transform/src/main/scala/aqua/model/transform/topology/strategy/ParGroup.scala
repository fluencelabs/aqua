package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.OnModel

import cats.Eval
import cats.syntax.apply.*
import cats.syntax.reducible.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.instances.lazyList.*

object ParGroup extends Begins with Ends {
  override def toString: String = "<par>"

  // Optimization: find the longest common prefix of all the par branches, and move it outside of this par
  // When branches will calculate their paths, they will take this move into account.
  // So less hops will be produced
  override def beginsOn(current: Topology): Eval[TopologyPath] =
    current.children
      .map(_.beginsOn.map(_.reverse))
      .reduceLeftOption { case (b1e, b2e) =>
        (b1e, b2e).mapN { case (b1, b2) => b1.commonPrefix(b2) }
      }
      .map(_.map(_.reverse)) getOrElse super.beginsOn(current)

  // Par block ends where all the branches end, if they have forced exit (not fire-and-forget)
  override def endsOn(current: Topology): Eval[TopologyPath] =
    current.children
      .traverse(_.forceExit)
      .flatMap(_.combineAll match {
        case ExitStrategy.Full => current.afterOn
        case ExitStrategy.Empty => super.endsOn(current)
      })
}
