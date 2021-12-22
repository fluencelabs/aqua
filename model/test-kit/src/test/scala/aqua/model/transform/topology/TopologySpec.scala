package aqua.model.transform.topology

import aqua.Node
import aqua.model.VarModel
import aqua.model.func.Call
import aqua.model.func.raw.FuncOps
import aqua.model.transform.res.{MakeRes, ResolvedOp, SeqRes, XorRes}
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
    val exportTo = Call.Export("result", ScalarType.string) :: Nil
    val result = VarModel("result", ScalarType.string)

    val init = on(
      initPeer,
      relay :: Nil,
      FuncOps.seq(
        FuncOps.par(
          on(
            otherPeer,
            otherRelay :: Nil,
            callTag(1, exportTo)
          ),
          callTag(2)
        ),
        callTag(3, Nil, result :: Nil)
      )
    )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        MakeRes.par(
          MakeRes.seq(
            through(relay),
            through(otherRelay),
            callRes(1, otherPeer, exportTo.headOption),
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

  // func registerKeyPutValue(node_id: string) -> []string:
  //  on node_id:
  //    nodes <- OpH.pr()
  //  on node_id:
  //      for n <- nodes par:
  //        on n:
  //          OpH.op("in")
  //  <- nodes
  // this example doesn't create a hop on relay after fold
  // but the test create it, so there is not a one-on-one simulation
  // change it or write an integration test
  "topology resolver" should "create returning hops on chain of 'on'" in {
    val init =
      on(
        initPeer,
        relay :: Nil,
        on(
          otherPeer,
          Nil,
          callTag(0)
        ),
        on(
          otherPeer,
          Nil,
          foldPar(
            "i",
            valueArray,
            on(
              otherPeer2,
              Nil,
              callTag(2)
            )
          )
        ),
        callTag(3)
      )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        callRes(0, otherPeer),
        MakeRes.fold("i", valueArray, MakeRes.par(callRes(2, otherPeer2), nextRes("i"))),
        through(relay),
        callRes(3, initPeer)
      )
    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "create returning hops on nested 'on'" in {
    val init =
      on(
        initPeer,
        relay :: Nil,
        callTag(0),
        on(
          otherPeer,
          Nil,
          callTag(1),
          fold(
            "i",
            valueArray,
            on(
              otherPeer2,
              otherRelay2 :: Nil,
              callTag(2)
            )
          )
        ),
        callTag(3)
      )

    val proc: Node.Res = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        callRes(0, initPeer),
        through(relay),
        callRes(1, otherPeer),
        through(otherRelay2),
        MakeRes.fold(
          "i",
          valueArray,
          callRes(2, otherPeer2),
          nextRes("i")
        ),
        through(otherRelay2),
        through(relay),
        callRes(3, initPeer)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  // https://github.com/fluencelabs/aqua/issues/205
  "topology resolver" should "optimize path over fold" in {
    val i = VarModel("i", ScalarType.string)
    val init =
      on(
        initPeer,
        relay :: Nil,
        fold(
          "i",
          valueArray,
          on(
            i,
            otherRelay :: Nil,
            callTag(1)
          )
        )
      )

    val proc = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        MakeRes.fold(
          "i",
          valueArray,
          MakeRes.seq(
            through(otherRelay),
            callRes(1, i)
          ),
          MakeRes.seq(
            through(otherRelay),
            nextRes("i")
          )
        )
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "handle detach" in {
    val init =
      on(
        initPeer,
        relay :: Nil,
        co(on(otherPeer, Nil, callTag(1, Call.Export(varNode.name, varNode.`type`) :: Nil))),
        callTag(2, Nil, varNode :: Nil)
      )

    val proc = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        MakeRes.par(
          MakeRes.seq(
            callRes(1, otherPeer, Some(Call.Export(varNode.name, varNode.`type`))),
            through(relay),
            through(initPeer) // pingback
          )
        ),
        callRes(2, initPeer, None, varNode :: Nil)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "handle moved detach" in {
    val init =
      on(
        initPeer,
        relay :: Nil,
        on(
          otherPeer2,
          Nil,
          co(on(otherPeer, Nil, callTag(1, Call.Export(varNode.name, varNode.`type`) :: Nil))),
          callTag(2, Nil, varNode :: Nil)
        )
      )

    val proc = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        MakeRes.par(
          MakeRes.seq(
            callRes(1, otherPeer, Some(Call.Export(varNode.name, varNode.`type`))),
            through(otherPeer2) // pingback
          )
        ),
        callRes(2, otherPeer2, None, varNode :: Nil)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "handle detach moved to relay" in {
    val init =
      on(
        initPeer,
        relay :: Nil,
        on(
          relay,
          Nil,
          co(on(otherPeer, Nil, callTag(1, Call.Export(varNode.name, varNode.`type`) :: Nil)))
        ),
        callTag(2, Nil, varNode :: Nil)
      )

    val proc = Topology.resolve(init)

    val expected: Node.Res =
      MakeRes.seq(
        through(relay),
        MakeRes.par(
          MakeRes.seq(
            callRes(1, otherPeer, Some(Call.Export(varNode.name, varNode.`type`))),
            through(relay), // pingback
            through(initPeer) // pingback
          )
        ),
        callRes(2, initPeer, None, varNode :: Nil)
      )

    proc.equalsOrPrintDiff(expected) should be(true)
  }

}
