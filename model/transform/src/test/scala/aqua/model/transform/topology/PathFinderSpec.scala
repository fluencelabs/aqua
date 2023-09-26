package aqua.model.transform.topology

import aqua.model.*
import aqua.model.transform.ModelBuilder
import aqua.types.ScalarType

import cats.data.Chain
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PathFinderSpec extends AnyFlatSpec with Matchers {

  val relay = ValueModel.fromRaw(ModelBuilder.relayV)
  val initPeer = ValueModel.fromRaw(ModelBuilder.initPeer)

  val relayOn = OnModel(relay, Chain.empty)
  val initPeerRelayOn = OnModel(initPeer, Chain.one(relay))

  it should "find path from (op, or) -> r -> (i, r) to r -> (i, r)" in {

    val otherPeer = VarModel("other-peer", ScalarType.string)
    val otherRelay = VarModel("other-relay", ScalarType.string)
    val otherPeerRelayOn = OnModel(otherPeer, Chain.one(otherRelay))

    val from = TopologyPath(otherPeerRelayOn :: relayOn :: initPeerRelayOn :: Nil)
    val to = TopologyPath(relayOn :: initPeerRelayOn :: Nil)

    val path = PathFinder.findPath(from, to)

    path shouldBe Chain.one(otherRelay)
  }

}
