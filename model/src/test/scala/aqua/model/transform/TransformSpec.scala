package aqua.model.transform

import aqua.model.func.body.{CallArrowTag, CallServiceTag, FuncOp}
import aqua.model.func.{ArgsDef, Call, FuncCallable}
import aqua.model.{LiteralModel, Node, VarModel}
import aqua.types.ScalarType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TransformSpec extends AnyFlatSpec with Matchers {
  import Node._

  "transform.forClient" should "work well with function 1 (no calls before on)" in {

    val ret = LiteralModel.quote("return this")

    val func: FuncCallable =
      FuncCallable(
        "ret",
        FuncOp(on(otherPeer, Nil, call(1))),
        ArgsDef.empty,
        Some((ret, ScalarType.string)),
        Map.empty,
        Map.empty
      )

    val bc = BodyConfig()

    val fc = Transform.forClient(func, bc)

    val procFC: Node = fc

    val expectedFC = seq(
      xor(
        seq(
          dataCall(bc, "-relay-", initPeer),
          through(relayV),
          xor(
            call(1, otherPeer),
            seq(
              through(relayV),
              errorCall(bc, 1, initPeer),
              through(relayV)
            )
          ),
          through(relayV),
          xor(
            respCall(bc, ret, initPeer),
            seq(
              errorCall(bc, 2, initPeer)
            )
          )
        ),
        seq(
          through(relayV),
          errorCall(bc, 3, initPeer)
        )
      )
    )

    procFC.equalsOrPrintDiff(expectedFC) should be(true)

  }

  "transform.forClient" should "work well with function 2 (with a call before on)" in {

    val ret = LiteralModel.quote("return this")

    val func: FuncCallable = FuncCallable(
      "ret",
      FuncOp(seq(call(0), on(otherPeer, Nil, call(1)))),
      ArgsDef.empty,
      Some((ret, ScalarType.string)),
      Map.empty,
      Map.empty
    )

    val bc = BodyConfig(wrapWithXor = false)

    val fc = Transform.forClient(func, bc)

    val procFC: Node = fc

    val expectedFC =
      seq(
        dataCall(bc, "-relay-", initPeer),
        call(0, initPeer),
        through(relayV),
        call(1, otherPeer),
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
              Call(Nil, Some(Call.Export("v", ScalarType.string))),
              None
            )
          )
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
          Node(CallArrowTag("callable", Call(Nil, Some(Call.Export("v", ScalarType.string)))))
        ),
        ArgsDef.empty,
        Some((VarModel("v", ScalarType.string), ScalarType.string)),
        Map("callable" -> f1),
        Map.empty
      )

    val bc = BodyConfig(wrapWithXor = false)

    val res = Transform.forClient(f2, bc): Node

    res.equalsOrPrintDiff(
      seq(
        dataCall(bc, "-relay-", initPeer),
        Node(
          CallServiceTag(
            LiteralModel.quote("srv1"),
            "foo",
            Call(Nil, Some(Call.Export("v", ScalarType.string))),
            Some(initPeer)
          )
        ),
        respCall(bc, VarModel("v", ScalarType.string), initPeer)
      )
    ) should be(true)
  }
}
