package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.{PathFinder, Topology}
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain
import cats.syntax.apply.*

trait Begins {

  def beginsOn(current: Topology): Eval[List[OnModel]] = current.pathOn

  def pathBefore(current: Topology): Eval[Chain[ValueModel]] =
    (current.beforeOn, current.beginsOn).mapN { case (bef, beg) =>
      (PathFinder.findPath(bef, beg), bef, beg)
    }.flatMap { case (pb, bef, beg) =>
      // Handle the case when we need to go through the relay, but miss the hop as it's the first
      // peer where we go, but there's no service calls there
      current.firstExecutesOn.map {
        case Some(where) if where != beg =>
          pb ++ Topology.findRelayPathEnforcement(bef, beg)
        case _ => pb
      }
    }
}
