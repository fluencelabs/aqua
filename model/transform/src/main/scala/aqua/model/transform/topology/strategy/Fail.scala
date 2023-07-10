package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.ValueModel

import aqua.model.{OnModel, XorModel}

import cats.data.Chain
import cats.Eval
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.option.*
import cats.syntax.applicative.*

object Fail extends Before with Begins with After {

  override def forceExit(current: Topology): Eval[Boolean] =
    Eval.now(false) // override just to be explicit

  override def beforeOn(current: Topology): Eval[List[OnModel]] =
    current.parent
      .map(_.cursor.op)
      .collect { case XorModel =>
        current.prevSibling.traverse(_.beginsOn)
      }
      .flatSequence
      .flatMap(
        _.fold(super.beforeOn(current))(_.pure)
      )

  override def pathBefore(current: Topology): Eval[Chain[ValueModel]] =
    for {
      path <- super.pathBefore(current)
      begins <- current.beginsOn
      hop = begins.headOption
        .map(_.peerId)
        .filterNot(peer =>
          path.lastOption.contains(peer) || path.isEmpty
        )
    } yield path ++ Chain.fromOption(hop)
}
