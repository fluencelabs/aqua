package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.{PathFinder, Topology, TopologyPath}
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.instances.tuple.*

trait Begins {

  def beginsOn(current: Topology): Eval[TopologyPath] = current.pathOn

  def pathBefore(current: Topology): Eval[Chain[ValueModel]] =
    (current.beforeOn, current.beginsOn).tupled
      .fproduct(PathFinder.findPath.tupled)
      .flatMap { case ((bef, beg), path) =>
        // Handle the case when we need to go through the relay, but miss the hop as it's the first
        // peer where we go, but there's no service calls there
        current.firstExecutesOn.map {
          case Some(where) if where != beg =>
            path ++ Topology.findRelayPathEnforcement(bef, beg)
          case _ => path
        }
      }
}
