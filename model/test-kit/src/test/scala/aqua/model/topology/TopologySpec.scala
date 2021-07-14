package aqua.model.topology

import aqua.Node
import aqua.model.VarModel
import aqua.model.func.Call
import aqua.model.func.raw.FuncOps
import aqua.model.func.resolved.{MakeRes, ResolvedOp, XorRes}
import aqua.types.ScalarType
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopologySpec extends AnyFlatSpec with Matchers {
  import Node._

  "topology resolver" should "do nothing on init peer" in {

    val init: Node.Raw = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        callTag(1),
        callTag(2)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        callRes(1, initPeer),
        callRes(2, initPeer)
      )

    proc should be(expected)

  }

  "topology resolver" should "go through relay to any other node, directly" in {

    val init: Node.Raw = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        Nil,
        FuncOps.seq(
          callTag(1),
          callTag(2)
        )
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(through(relay), callRes(1, otherPeer), callRes(2, otherPeer))

    proc should be(expected)
  }

  "topology resolver" should "go through relay to any other node, via another relay" in {

    val init = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        otherRelay :: Nil,
        FuncOps.seq(
          callTag(1),
          callTag(2)
        )
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        through(otherRelay),
        callRes(1, otherPeer),
        callRes(2, otherPeer)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "build return path in par if there are exported variables" in {
    val export = Some(Call.Export("result", ScalarType.string))
    val result = VarModel("result", ScalarType.string)

    val init = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        FuncOps.par(
          on(
            otherPeer,
            otherRelay :: Nil,
            callTag(1, export)
          ),
          callTag(2)
        ),
        callTag(3, None, result :: Nil)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        MakeRes.par(
          MakeRes.seq(
            through(relay),
            through(otherRelay),
            callRes(1, otherPeer, export),
            through(otherRelay),
            through(relay),
            // we should return to a caller to continue execution
            through(initPeer)
          ),
          callRes(2, initPeer)
        ),
        callRes(3, initPeer, None, result :: Nil)
      )

    Node.equalsOrPrintDiff(proc, expected) should be(true)
  }

  "topology resolver" should "work fine with par" in {
    val init = on(
      initPeer,
      relay :: Nil,
      FuncOps.par(
        on(
          otherPeer,
          otherRelay :: Nil,
          callTag(1)
        ),
        callTag(2)
      )
    )

    val proc = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.par(
        MakeRes.seq(
          through(relay),
          through(otherRelay),
          callRes(1, otherPeer)
        ),
        callRes(2, initPeer)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "create correct calls in try" in {
    val init = Node.`try`(callTag(1))

    val proc = Topology.resolve(init)

    proc.equalsOrPrintDiff(
      Cofree[Chain, ResolvedOp](XorRes, Eval.now(Chain.one(callRes(1, initPeer))))
    ) should be(true)
  }

  "topology resolver" should "work fine with par with on" in {
    val init: Node.Raw = on(
      initPeer,
      relay :: Nil,
      FuncOps.par(
        on(
          otherPeer,
          otherRelay :: Nil,
          callTag(1)
        ),
        on(
          otherPeer2,
          otherRelay2 :: Nil,
          callTag(2)
        )
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.par(
        MakeRes.seq(
          through(relay),
          through(otherRelay),
          callRes(1, otherPeer)
        ),
        MakeRes.seq(
          through(relay),
          through(otherRelay2),
          callRes(2, otherPeer2)
        )
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "go through relay to any other node, via another relay, in complex xor/seq" in {

    val init = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        otherRelay :: Nil,
        FuncOps.xor(
          FuncOps.seq(
            callTag(1),
            callTag(2)
          ),
          callTag(3)
        )
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        through(otherRelay),
        MakeRes.xor(
          MakeRes.seq(
            callRes(1, otherPeer),
            callRes(2, otherPeer)
          ),
          callRes(3, otherPeer)
        )
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "simplify a route with init_peer_id" in {
    val init: Node.Raw = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        on(
          initPeer,
          relay :: Nil,
          callTag(1)
        ),
        callTag(2)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected =
      MakeRes.seq(
        callRes(1, initPeer),
        callRes(2, initPeer)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "get back to init peer" in {

    val init: Node.Raw = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          callTag(1)
        ),
        callTag(2)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        through(otherRelay),
        callRes(1, otherPeer),
        through(otherRelay),
        through(relay),
        callRes(2, initPeer)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "not stackoverflow" in {
    val init = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        callTag(1),
        callTag(2),
        callTag(3),
        on(
          varNode,
          viaList :: Nil,
          callTag(4)
        ),
        on(
          initPeer,
          relay :: Nil,
          callTag(5)
        )
      )
    )

    Topology.resolve(init)
  }

  "topology resolver" should "get back to init peer after a long chain" in {
    val init = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          callTag(0),
          on(
            otherPeer2,
            otherRelay :: Nil,
            callTag(1),
            matchRaw(
              otherPeer,
              otherRelay,
              on(
                otherPeer,
                otherRelay :: Nil,
                callTag(2)
              )
            )
          )
        ),
        callTag(3)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        through(otherRelay),
        callRes(0, otherPeer),
        through(otherRelay),
        callRes(1, otherPeer2),
        matchRes(
          otherPeer,
          otherRelay,
          MakeRes.seq(
            through(otherRelay),
            callRes(2, otherPeer)
          )
        ),
        through(otherRelay),
        through(relay),
        callRes(3, initPeer)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "resolve xor path" in {

    val init = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        FuncOps.xor(
          on(
            otherPeer,
            otherRelay :: Nil,
            callTag(0)
          ),
          on(
            initPeer,
            relay :: Nil,
            callTag(1)
          )
        ),
        on(
          otherPeer,
          otherRelay :: Nil,
          callTag(3)
        ),
        callTag(4)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        MakeRes.xor(
          MakeRes.seq(
            through(relay),
            through(otherRelay),
            callRes(0, otherPeer)
          ),
          MakeRes.seq(
            through(otherRelay),
            through(relay),
            callRes(1, initPeer),
            through(relay),
            through(otherRelay)
          )
        ),
        callRes(3, otherPeer),
        through(otherRelay),
        through(relay),
        callRes(4, initPeer)
      )

    Node.equalsOrPrintDiff(proc, expected) should be(true)
  }

}
