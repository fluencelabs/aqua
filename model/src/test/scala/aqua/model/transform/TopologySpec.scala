package aqua.model.transform

import aqua.model.body.FuncOp
import aqua.model.{FuncCallable, FuncResolved, InitPeerIdModel, LiteralModel, Node}
import aqua.types.{LiteralType, ScalarType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopologySpec extends AnyFlatSpec with Matchers {
  import Node._

  "topology resolver" should "do nothing on init peer" in {

    val init = on(
      initPeer,
      relay :: Nil,
      seq(
        call(1),
        call(2)
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      seq(
        call(1, initPeer),
        call(2, initPeer)
      )
    )

    proc should be(expected)

  }

  "topology resolver" should "go through relay to any other node, directly" in {

    val init = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        Nil,
        seq(
          call(1),
          call(2)
        )
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        Nil,
        through(relay),
        seq(
          call(1, otherPeer),
          call(2, otherPeer)
        )
      )
    )

    proc should be(expected)
  }

  "topology resolver" should "go through relay to any other node, via another relay" in {

    val init = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        otherRelay :: Nil,
        seq(
          call(1),
          call(2)
        )
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        otherRelay :: Nil,
        through(relay),
        through(otherRelay),
        seq(
          call(1, otherPeer),
          call(2, otherPeer)
        )
      )
    )

    proc should be(expected)
  }

  "topology resolver" should "get back to init peer" in {

    val init = on(
      initPeer,
      relay :: Nil,
      seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          call(1)
        ),
        call(2)
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          through(relay),
          through(otherRelay),
          call(1, otherPeer)
        ),
        through(otherRelay),
        through(relay),
        call(2, initPeer)
      )
    )

//    println(Console.BLUE + init)
//    println(Console.YELLOW + proc)
//    println(Console.MAGENTA + expected)
//    println(Console.RESET)

    proc should be(expected)
  }

  "topology resolver" should "work well with function 1 (no calls before on)" in {

    val ret = LiteralModel("\"return this\"")

    val func: FuncResolved = FuncResolved(
      "ret",
      FuncCallable(
        FuncOp(on(otherPeer, Nil, call(1))),
        Nil,
        Some(ret -> ScalarType.string),
        Map.empty
      )
    )

    val bc = BodyConfig()

    val fc = ForClient(func, bc)

    val procFC: Node = fc

    val expectedFC =
      xor(
        on(
          initPeer,
          relayV :: Nil,
          seq(
            dataCall(bc, "relay", initPeer),
            on(otherPeer, Nil, through(relayV), call(1, otherPeer)),
            through(relayV),
            on(initPeer, relayV :: Nil, respCall(bc, ret, initPeer))
          )
        ),
        on(initPeer, relayV :: Nil, xorErrorCall(bc, initPeer))
      )

    procFC.equalsOrPrintDiff(expectedFC) should be(true)

  }

  "topology resolver" should "work well with function 2 (with a call before on)" in {

    val ret = LiteralModel("\"return this\"")

    val func: FuncResolved = FuncResolved(
      "ret",
      FuncCallable(
        FuncOp(seq(call(0), on(otherPeer, Nil, call(1)))),
        Nil,
        Some(ret -> ScalarType.string),
        Map.empty
      )
    )

    val bc = BodyConfig()

    val fc = ForClient(func, bc)

    val procFC: Node = fc

    val expectedFC =
      xor(
        on(
          initPeer,
          relayV :: Nil,
          seq(
            dataCall(bc, "relay", initPeer),
            seq(
              call(0, initPeer),
              on(otherPeer, Nil, through(relayV), call(1, otherPeer))
            ),
            on(initPeer, relayV :: Nil, through(relayV), respCall(bc, ret, initPeer))
          )
        ),
        on(initPeer, relayV :: Nil, xorErrorCall(bc, initPeer))
      )

    procFC.equalsOrPrintDiff(expectedFC) should be(true)

  }

}
