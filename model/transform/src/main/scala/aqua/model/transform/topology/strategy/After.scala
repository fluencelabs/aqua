package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.{PathFinder, Topology}
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain
import cats.syntax.apply.*
import aqua.model.transform.topology.TopologyPath

trait After {
  def forceExit(current: Topology): Eval[ExitStrategy] = Eval.now(ExitStrategy.Empty)

  def afterOn(current: Topology): Eval[TopologyPath] = current.pathOn

  protected def afterParent(current: Topology): Eval[TopologyPath] =
    current.parent.map(
      _.afterOn
    ) getOrElse current.pathOn

  // In case exit is performed and pathAfter is inserted, we're actually where
  // execution is expected to continue After this node is handled
  final def finallyOn(current: Topology): Eval[TopologyPath] =
    current.forceExit.flatMap {
      case ExitStrategy.Full => current.afterOn
      case ExitStrategy.ToRelay => current.relayOn
      case ExitStrategy.Empty => current.endsOn
    }

  // If exit is forced, make a path outside this node
  // – from where it ends to where execution is expected to continue
  def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    pathAfterVia(current)

  // If exit is forced, make a path outside this node
  // – from where it ends to where execution is expected to continue
  private def pathAfterVia(current: Topology): Eval[Chain[ValueModel]] =
    current.forceExit.flatMap {
      case ExitStrategy.Empty => Eval.now(Chain.empty)
      case ExitStrategy.ToRelay =>
        (current.endsOn, current.relayOn).mapN(PathFinder.findPathEnforce)
      case ExitStrategy.Full =>
        (current.endsOn, current.afterOn).mapN(PathFinder.findPath)
    }

  // If exit is forced, make a path outside this node
  // – from where it ends to where execution is expected to continue,
  // explicitly pinging the next node (useful inside par branches)
  def pathAfterAndPingNext(current: Topology): Eval[Chain[ValueModel]] =
    current.forceExit.flatMap {
      case ExitStrategy.Empty | ExitStrategy.ToRelay => Eval.now(Chain.empty)
      case ExitStrategy.Full =>
        (current.endsOn, current.afterOn, current.lastExecutesOn).mapN {
          case (e, a, _) if e == a => Chain.empty
          case (e, a, l) if l.contains(e) =>
            // Pingback in case no relays involved
            Chain.fromOption(
              a.current
                // Add nothing if last node is the same
                .filterNot(e.current.contains)
                .map(_.peerId)
            )
          case (e, a, _) =>
            // We wasn't at e, so need to get through the last peer in case it matches with the relay
            Topology.findRelayPathEnforcement(a, e) ++ Chain.fromOption(
              a.peerId
            )
        }
    }.flatMap { appendix =>
      // Ping the next (join) peer to enforce its data update
      pathAfterVia(current).map(_ ++ appendix)
    }
}
