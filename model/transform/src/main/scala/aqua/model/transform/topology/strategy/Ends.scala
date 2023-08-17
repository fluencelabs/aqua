package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.{PathFinder, Topology}
import aqua.model.transform.topology.TopologyPath
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.OnModel

import cats.Eval

trait Ends {

  def endsOn(current: Topology): Eval[TopologyPath] =
    current.beginsOn

  private def childFinally(
    current: Topology,
    child: Topology => Option[Topology]
  ): Eval[TopologyPath] =
    child(current).map(lc =>
      lc.forceExit.flatMap {
        case ExitStrategy.Empty => lc.endsOn
        case ExitStrategy.ToRelay => lc.pathOn.map(_.toRelay)
        case ExitStrategy.Full => current.afterOn
      }
    ) getOrElse current.beginsOn

  protected def lastChildFinally(current: Topology): Eval[TopologyPath] =
    childFinally(current, _.lastChild)

  protected def firstChildFinally(current: Topology): Eval[TopologyPath] =
    childFinally(current, _.firstChild)
}
