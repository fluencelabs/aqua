package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.OnModel

import cats.Eval
import cats.syntax.apply.*

object ParGroup extends Begins with Ends {
  override def toString: String = "<par>"

  // Optimization: find the longest common prefix of all the par branches, and move it outside of this par
  // When branches will calculate their paths, they will take this move into account.
  // So less hops will be produced
  override def beginsOn(current: Topology): Eval[List[OnModel]] =
    current.children
      .map(_.beginsOn.map(_.reverse))
      .reduceLeftOption { case (b1e, b2e) =>
        (b1e, b2e).mapN { case (b1, b2) =>
          (b1 zip b2).takeWhile(_ == _).map(_._1)
        }
      }
      .map(_.map(_.reverse)) getOrElse super.beginsOn(current)

  // Par block ends where all the branches end, if they have forced exit (not fire-and-forget)
  override def endsOn(current: Topology): Eval[List[OnModel]] =
    current.children
      .map(_.forceExit)
      .reduceLeftOption { case (a, b) =>
        (a, b).mapN(_ || _)
      }
      .map(_.flatMap {
        case true => current.afterOn
        case false => super.endsOn(current)
      }) getOrElse super.endsOn(current)
}
