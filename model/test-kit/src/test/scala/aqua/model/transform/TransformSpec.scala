package aqua.model.transform

import aqua.Node
import aqua.raw.arrow.FuncArrow
import aqua.model.transform.res.{CallRes, CallServiceRes, MakeRes}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{LiteralModel, VarModel}
import aqua.raw.ops.{Call, CallArrowTag, CallServiceTag, FuncOp, FuncOps}
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.types.{ArrowType, NilType, ProductType, ScalarType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TransformSpec extends AnyFlatSpec with Matchers {
  import Node.*

  val stringArrow: ArrowType = ArrowType(NilType, ProductType(ScalarType.string :: Nil))

  "transform.forClient" should "work well with function 1 (no calls before on), generate correct error handling" in {

    val ret = LiteralRaw.quote("return this")

    val func: FuncArrow =
      FuncArrow(
        "ret",
        on(otherPeer, otherRelay :: Nil, callTag(1)),
        stringArrow,
        ret :: Nil,
        Map.empty,
        Map.empty
      )

    val bc = TransformConfig()

    val fc = Transform.funcRes(func, bc)

    val procFC: Node.Res = fc.body

    val expectedFC: Node.Res =
      MakeRes.xor(
        MakeRes.seq(
          dataCall(bc, "-relay-", initPeer),
          through(relayV),
          through(otherRelay),
          MakeRes.xor(
            MakeRes.seq(
              callRes(1, otherPeer),
              through(otherRelay),
              through(relayV)
            ),
            MakeRes.seq(
              through(otherRelay),
              through(relayV),
              errorCall(bc, 1, initPeer)
            )
          ),
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

    val ret = LiteralRaw.quote("return this")

    val func: FuncArrow = FuncArrow(
      "ret",
      FuncOps.seq(callTag(0), on(otherPeer, Nil, callTag(1))),
      stringArrow,
      ret :: Nil,
      Map.empty,
      Map.empty
    )

    val bc = TransformConfig(wrapWithXor = false)

    val fc = Transform.funcRes(func, bc)

    val procFC: Res = fc.body

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

    val f1: FuncArrow =
      FuncArrow(
        "f1",
        FuncOp(
          Node(
            CallServiceTag(
              LiteralRaw.quote("srv1"),
              "foo",
              Call(Nil, Call.Export("v", ScalarType.string) :: Nil)
            )
          ).cof
        ),
        stringArrow,
        VarRaw("v", ScalarType.string) :: Nil,
        Map.empty,
        Map.empty
      )

    val f2: FuncArrow =
      FuncArrow(
        "f2",
        FuncOp(
          Node(CallArrowTag("callable", Call(Nil, Call.Export("v", ScalarType.string) :: Nil))).cof
        ),
        stringArrow,
        VarRaw("v", ScalarType.string) :: Nil,
        Map("callable" -> f1),
        Map.empty
      )

    val bc = TransformConfig(wrapWithXor = false)

    val res = Transform.funcRes(f2, bc).body: Node.Res

    res.equalsOrPrintDiff(
      MakeRes.seq(
        dataCall(bc, "-relay-", initPeer),
        Node(
          CallServiceRes(
            LiteralRaw.quote("srv1"),
            "foo",
            CallRes(Nil, Some(Call.Export("v", ScalarType.string))),
            initPeer
          )
        ),
        respCall(bc, VarRaw("v", ScalarType.string), initPeer)
      )
    ) should be(true)
  }
}
