package aqua.model.transform

import aqua.model.transform.ModelBuilder
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{CallModel, FuncArrow, LiteralModel, VarModel}
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp, OnTag, RawTag, SeqTag}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.res.{CallRes, CallServiceRes, MakeRes, SeqRes, XorRes}
import aqua.types.{ArrowType, NilType, ProductType, ScalarType}

import cats.data.Chain
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TransformSpec extends AnyFlatSpec with Matchers {

  import ModelBuilder.*

  val stringArrow: ArrowType = ArrowType(NilType, ProductType(ScalarType.string :: Nil))

  def callOp(i: Int, exportTo: List[Call.Export] = Nil, args: List[ValueRaw] = Nil): RawTag =
    CallArrowRawTag.service(
      VarRaw(s"srv$i", ScalarType.string),
      s"fn$i",
      Call(args, exportTo)
    )

  "transform.forClient" should "work well with function 1 (no calls before on), generate correct error handling" in {

    val ret = LiteralRaw.quote("return this")

    val func: FuncArrow =
      FuncArrow(
        "ret",
        OnTag(otherPeer, Chain.fromSeq(otherRelay :: Nil)).wrap(callOp(1).leaf),
        stringArrow,
        ret :: Nil,
        Map.empty,
        Map.empty,
        None
      )

    val bc = TransformConfig()

    val fc = Transform.funcRes(func, bc)

    val procFC = fc.value.body

    val expectedFC = XorRes.wrap(
      SeqRes.wrap(
        dataCall(bc, "-relay-", initPeer),
        XorRes.wrap(
          SeqRes.wrap(
            through(relayV),
            through(otherRelay),
            callRes(1, otherPeer),
            through(otherRelay),
            through(relayV)
          ),
          SeqRes.wrap(
            through(otherRelay),
            through(relayV),
            through(initPeer),
            failErrorRes
          )
        ),
        respCall(bc, ret, initPeer)
      ),
      errorCall(bc, 0, initPeer)
    )

    procFC.equalsOrShowDiff(expectedFC) should be(true)

  }

  "transform.forClient" should "work well with function 2 (with a call before on)" in {

    val ret = LiteralRaw.quote("return this")

    /**
     * func ret() -> string:
     *   srv0.fn0()
     *   on "other-peer":
     *     srv1.fn1()
     *   <- "return this"
     */
    val func: FuncArrow = FuncArrow(
      "ret",
      SeqTag.wrap(
        callOp(0).leaf,
        OnTag(otherPeer, Chain.empty).wrap(
          callOp(1).leaf
        )
      ),
      stringArrow,
      ret :: Nil,
      Map.empty,
      Map.empty,
      None
    )

    val bc = TransformConfig()

    val fc = Transform.funcRes(func, bc)

    val procFC = fc.value.body

    val expectedFC = XorRes.wrap(
      SeqRes.wrap(
        dataCall(bc, "-relay-", initPeer),
        callRes(0, initPeer),
        XorRes.wrap(
          SeqRes.wrap(
            through(relayV),
            callRes(1, otherPeer),
            through(relayV)
          ),
          SeqRes.wrap(
            through(relayV),
            through(initPeer),
            failErrorRes
          )
        ),
        respCall(bc, ret, initPeer)
      ),
      errorCall(bc, 0, initPeer)
    )

    procFC.equalsOrShowDiff(expectedFC) should be(true)

  }

  "transform.forClient" should "link funcs correctly" in {
    /*
    func f1() -> string:
      v <- srv1.fn1()
      <- v

    func f2() -> string:
      v <- f1()
      <- v
     */

    val f1: FuncArrow =
      FuncArrow(
        "f1",
        callOp(1).leaf,
        stringArrow,
        VarRaw("v", ScalarType.string) :: Nil,
        Map.empty,
        Map.empty,
        None
      )

    val f2: FuncArrow =
      FuncArrow(
        "f2",
        CallArrowRawTag
          .func("callable", Call(Nil, Call.Export("v", ScalarType.string) :: Nil))
          .leaf,
        stringArrow,
        VarRaw("v", ScalarType.string) :: Nil,
        Map("callable" -> f1),
        Map.empty,
        None
      )

    val bc = TransformConfig()

    val procFC = Transform.funcRes(f2, bc).value.body

    val expectedFC = XorRes.wrap(
      SeqRes.wrap(
        dataCall(bc, "-relay-", initPeer),
        callRes(1, initPeer),
        respCall(bc, VarRaw("v", ScalarType.string), initPeer)
      ),
      errorCall(bc, 0, initPeer)
    )

    procFC.equalsOrShowDiff(expectedFC) should be(true)
  }

}
