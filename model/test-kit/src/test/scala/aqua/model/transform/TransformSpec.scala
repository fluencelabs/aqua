package aqua.model.transform

import aqua.Node
import aqua.model.func.raw.{CallArrowTag, CallServiceTag, FuncOp, FuncOps}
import aqua.model.func.resolved.{CallServiceRes, MakeRes}
import aqua.model.func.{ArgsDef, Call, FuncCallable}
import aqua.model.{LiteralModel, VarModel}
import aqua.types.ScalarType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TransformSpec extends AnyFlatSpec with Matchers {
  import Node._

  "transform.forClient" should "work well with function 1 (no calls before on), generate correct error handling" in {

    val ret = LiteralModel.quote("return this")

    val func: FuncCallable =
      FuncCallable(
        "ret",
        on(otherPeer, otherRelay :: Nil, callTag(1)),
        ArgsDef.empty,
        Some((ret, ScalarType.string)),
        Map.empty,
        Map.empty
      )

    val bc = BodyConfig()

    val fc = Transform.forClient(func, bc)

    val procFC: Node.Res = fc

    val expectedFC: Node.Res =
      MakeRes.xor(
        MakeRes.seq(
          dataCall(bc, "-relay-", initPeer),
          through(relayV),
          through(otherRelay),
          MakeRes.xor(
            callRes(1, otherPeer),
            MakeRes.seq(
              through(otherRelay),
              through(relayV),
              errorCall(bc, 1, initPeer),
              through(relayV)
            )
          ),
          through(otherRelay),
          through(relayV),
          MakeRes.xor(
            respCall(bc, ret, initPeer),
            errorCall(bc, 2, initPeer)
          )
        ),
        errorCall(bc, 3, initPeer)
      )

    //println(procFC)

    Node.equalsOrPrintDiff(procFC, expectedFC) should be(true)

  }

  "transform.forClient" should "work well with function 2 (with a call before on)" in {

    val ret = LiteralModel.quote("return this")

    val func: FuncCallable = FuncCallable(
      "ret",
      FuncOps.seq(callTag(0), on(otherPeer, Nil, callTag(1))),
      ArgsDef.empty,
      Some((ret, ScalarType.string)),
      Map.empty,
      Map.empty
    )

    val bc = BodyConfig(wrapWithXor = false)

    val fc = Transform.forClient(func, bc)

    val procFC: Res = fc

    val expectedFC: Res =
      MakeRes.seq(
        dataCall(bc, "-relay-", initPeer),
        callRes(0, initPeer),
        through(relayV),
        callRes(1, otherPeer),
        through(relayV),
        respCall(bc, ret, initPeer)
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
        FuncOp(
          Node(
            CallServiceTag(
              LiteralModel.quote("srv1"),
              "foo",
              Call(Nil, Some(Call.Export("v", ScalarType.string)))
            )
          ).cof
        ),
        ArgsDef.empty,
        Some((VarModel("v", ScalarType.string), ScalarType.string)),
        Map.empty,
        Map.empty
      )

    val f2: FuncCallable =
      FuncCallable(
        "f2",
        FuncOp(
          Node(CallArrowTag("callable", Call(Nil, Some(Call.Export("v", ScalarType.string))))).cof
        ),
        ArgsDef.empty,
        Some((VarModel("v", ScalarType.string), ScalarType.string)),
        Map("callable" -> f1),
        Map.empty
      )

    val bc = BodyConfig(wrapWithXor = false)

    val res = Transform.forClient(f2, bc): Node.Res

    res.equalsOrPrintDiff(
      MakeRes.seq(
        dataCall(bc, "-relay-", initPeer),
        Node(
          CallServiceRes(
            LiteralModel.quote("srv1"),
            "foo",
            Call(Nil, Some(Call.Export("v0", ScalarType.string))),
            initPeer
          )
        ),
        respCall(bc, VarModel("v0", ScalarType.string), initPeer)
      )
    ) should be(true)
  }
}
