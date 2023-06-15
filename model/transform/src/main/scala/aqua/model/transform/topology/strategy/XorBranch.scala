package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.{OnModel, ParGroupModel, SeqGroupModel, ValueModel}

import cats.Eval
import cats.data.Chain

// Parent == Xor
object XorBranch extends Before with After {
  override def toString: String = Console.RED + "<xor>/*" + Console.RESET

  override def beforeOn(current: Topology): Eval[List[OnModel]] =
    current.prevSibling.map(_.endsOn) getOrElse super.beforeOn(current)

  private def closestParExit(current: Topology): Option[Topology] =
    current.parents
      .map(t => t -> t.parent.map(_.cursor.op))
      .takeWhile {
        case (t, Some(_: ParGroupModel)) => true
        case (t, Some(_: SeqGroupModel)) => t.nextSibling.isEmpty
        case _ => false
      }
      .map(_._1)
      .map(t => t -> t.cursor.op)
      .collectFirst { case (t, _: ParGroupModel) =>
        // println(Console.GREEN + s"collect ${t}" + Console.RESET)
        t
      }

  override def forceExit(current: Topology): Eval[Boolean] =
    closestParExit(current)
      .fold(Eval.later(current.cursor.moveUp.exists(_.hasExecLater)))(_.forceExit)

  override def afterOn(current: Topology): Eval[List[OnModel]] =
    current.forceExit.flatMap {
      case true =>
        closestParExit(current).fold(afterParent(current))(_.afterOn)
      case false => super.afterOn(current)
    }

  // Parent of this branch's parent xor – fixes the case when this xor is in par
  override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    closestParExit(current).fold(super.pathAfter(current))(_ => pathAfterAndPingNext(current))
}
