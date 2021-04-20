package aqua.model.transform

import aqua.model.{LiteralModel, Node, VarModel}
import aqua.model.func.{ArgsDef, Call, FuncCallable}
import aqua.model.func.body.{CallFunctionTag, CallServiceTag, FuncOp}
import aqua.types.ScalarType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TransformSpec extends AnyFlatSpec with Matchers {
  import Node._

  "transform.forClient" should "work well with function 1 (no calls before on)" in {

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

    val fc = Transform.forClient(func, bc)

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

  "transform.forClient" should "work well with function 2 (with a call before on)" in {

    val ret = LiteralModel("\"return this\"")

    val func: FuncCallable = FuncCallable(
      "ret",
      FuncOp(seq(call(0), on(otherPeer, Nil, call(1)))),
      ArgsDef.empty,
      Some(Call.Arg(ret, ScalarType.string)),
      Map.empty
    )

    val bc = BodyConfig()

    val fc = Transform.forClient(func, bc)

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

  "transform.forClient" should "link funcs correctly" in {
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
          Node(CallFunctionTag("callable", Call(Nil, Some("v"))))
        ),
        ArgsDef.empty,
        Some(Call.Arg(VarModel("v"), ScalarType.string)),
        Map("callable" -> f1)
      )

    val bc = BodyConfig(wrapWithXor = false)

    val res = Transform.forClient(f2, bc): Node

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
