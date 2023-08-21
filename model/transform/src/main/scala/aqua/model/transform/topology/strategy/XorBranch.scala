package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.{OnModel, ParGroupModel, SeqGroupModel, ValueModel, XorModel}

import cats.Eval
import cats.data.Chain
import cats.syntax.functor.*
import cats.instances.lazyList.*
import cats.syntax.option.*

// Parent == Xor
object XorBranch extends Before with After {
  override def toString: String = Console.RED + "<xor>/*" + Console.RESET

  override def beforeOn(current: Topology): Eval[TopologyPath] =
    current.prevSibling.map(_.beginsOn) getOrElse super.beforeOn(current)

  // Find closest par exit up and return its branch current is in
  // Returns none if there is no par up
  //                 or current is not at its exit
  private def closestParExitChild(current: Topology): Option[Topology] =
    current.parents
      .fproduct(_.parent.map(_.cursor.op))
      .dropWhile {
        case (t, Some(_: SeqGroupModel)) =>
          t.nextSibling.isEmpty
        case (_, Some(XorModel)) =>
          true
        case _ => false
      }
      .headOption
      .collect { case (t, Some(_: ParGroupModel)) => t }

  private def closestParExit(current: Topology): Option[Topology] =
    closestParExitChild(current).flatMap(_.parent)

  override def forceExit(current: Topology): Eval[ExitStrategy] =
    closestParExitChild(current).fold(
      Eval.later {
        if (current.cursor.moveUp.exists(_.hasExecLater)) ExitStrategy.Full
        else ExitStrategy.Empty
      }
    )(_.forceExit) // Force exit if par branch needs it

  override def afterOn(current: Topology): Eval[TopologyPath] =
    current.forceExit.flatMap {
      case ExitStrategy.Empty => super.afterOn(current)
      case ExitStrategy.ToRelay => current.relayOn
      case ExitStrategy.Full =>
        closestParExit(current).fold(afterParent(current))(_.afterOn)
    }

  // Parent of this branch's parent xor – fixes the case when this xor is in par
  override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    closestParExit(current).fold(super.pathAfter(current))(_ =>
      // Ping next if we are exiting from par
      super.pathAfterAndPingNext(current)
    )
}
