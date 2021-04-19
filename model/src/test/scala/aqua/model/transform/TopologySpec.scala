package aqua.model.transform

import aqua.model
import aqua.model.func.body.{CallArrowTag, CallServiceTag, FuncOp}
import aqua.model.func.{ArgsDef, Call, FuncCallable}
import aqua.model.{LiteralModel, Node, VarModel}
import aqua.types.ScalarType
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

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "work well with function 1 (no calls before on)" in {

    val ret = LiteralModel("\"return this\"")

    val func: FuncCallable =
      FuncCallable(
        "ret",
        FuncOp(on(otherPeer, Nil, call(1))),
        ArgsDef.empty,
        Some(Call.Arg(ret, ScalarType.string)),
        Map.empty
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

    val func: FuncCallable = FuncCallable(
      "ret",
      FuncOp(seq(call(0), on(otherPeer, Nil, call(1)))),
      ArgsDef.empty,
      Some(Call.Arg(ret, ScalarType.string)),
      Map.empty
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
            through(relayV),
            on(initPeer, relayV :: Nil, respCall(bc, ret, initPeer))
          )
        ),
        on(initPeer, relayV :: Nil, xorErrorCall(bc, initPeer))
      )

    procFC.equalsOrPrintDiff(expectedFC) should be(true)

  }

  "topology resolver" should "link funcs correctly" in {
    /*
    func one() -> u64:
      variable <- Demo.get42()
      <- variable

    func two() -> u64:
      variable <- one()
      <- variable
     */

    val f1: FuncCallable =
      FuncCallable(
        "f1",
        FuncOp(Node(CallServiceTag(LiteralModel("\"srv1\""), "foo", Call(Nil, Some("v")), None))),
        ArgsDef.empty,
        Some(Call.Arg(VarModel("v"), ScalarType.string)),
        Map.empty
      )

    val f2: FuncCallable =
      FuncCallable(
        "f2",
        FuncOp(
          Node(CallArrowTag("callable", Call(Nil, Some("v"))))
        ),
        ArgsDef.empty,
        Some(Call.Arg(VarModel("v"), ScalarType.string)),
        Map("callable" -> f1)
      )

    val bc = BodyConfig(wrapWithXor = false)

    val res = ForClient(f2, bc): Node

    res.equalsOrPrintDiff(
      on(
        initPeer,
        relayV :: Nil,
        seq(
          dataCall(bc, "relay", initPeer),
          Node(
            CallServiceTag(LiteralModel("\"srv1\""), "foo", Call(Nil, Some("v")), Some(initPeer))
          ),
          on(
            initPeer,
            relayV :: Nil,
            respCall(bc, VarModel("v"), initPeer)
          )
        )
      )
    ) should be(true)
  }

}
