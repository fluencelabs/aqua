package aqua.model.transform.topology

import aqua.raw.ops.FuncOps
import aqua.model.transform.cursor.ChainZipper
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.raw.ops.{Call, FuncOp, OnTag, ReturnTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, ScalarType}
import cats.data.{Chain, NonEmptyList}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RawCursorSpec extends AnyFlatSpec with Matchers {
  import FuncOp.*
  import aqua.raw.ops.FuncOps.*

  "simple raw cursor on init_peer_id" should "move properly" in {
    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            ValueRaw.InitPeerId,
            Chain.empty,
            callService(LiteralRaw.quote("calledOutside"), "fn", Call(Nil, Nil))
          ).tree
        )
      )
    )

    //raw.firstExecuted shouldBe raw.lastExecuted
  }

  "simple raw cursor with multiple calls" should "move on seqs" in {
    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            ValueRaw.InitPeerId,
            Chain.empty,
            seq(
              callService(LiteralRaw.quote("1"), "fn", Call(Nil, Nil)),
              callService(LiteralRaw.quote("2"), "fn", Call(Nil, Nil)),
              callService(LiteralRaw.quote("3"), "fn", Call(Nil, Nil)),
              callService(LiteralRaw.quote("4"), "fn", Call(Nil, Nil))
            )
          ).tree
        )
      )
    )

//    raw.lastExecuted shouldBe raw.firstExecuted.get.seqNext.get.seqNext.get.seqNext
//    raw.lastExecuted.get.seqPrev shouldBe raw.firstExecuted.get.seqNext.get.seqNext
//    raw.lastExecuted.get.seqPrev.get.seqPrev shouldBe raw.firstExecuted.get.seqNext
//    raw.lastExecuted.get.seqPrev shouldBe raw.firstExecuted.get.seqNext.get.seqNext
  }

  "simple raw cursor on init_peer_id via relay" should "move properly" in {
    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            ValueRaw.InitPeerId,
            Chain.one(VarRaw("-relay-", ScalarType.string)),
            callService(LiteralRaw.quote("calledOutside"), "fn", Call(Nil, Nil))
          ).tree
        )
      )
    )

    //raw.firstExecuted shouldBe raw.lastExecuted
  }

  "raw cursor" should "move properly" in {

    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            ValueRaw.InitPeerId,
            Chain.one(VarRaw("-relay-", ScalarType.string)),
            seq(
              callService(LiteralRaw.quote("calledOutside"), "fn", Call(Nil, Nil)),
              onVia(
                VarRaw("-other-", ScalarType.string),
                Chain.one(VarRaw("-external-", ScalarType.string)),
                seq(
                  callService(
                    LiteralRaw.quote("calledInside"),
                    "fn",
                    Call(Nil, Call.Export("export", ScalarType.string) :: Nil)
                  ),
                  leaf(ReturnTag(NonEmptyList.one(VarRaw("export", ScalarType.string))))
                )
              ),
              callService(
                LiteralRaw.quote("return"),
                "fn",
                Call(VarRaw("export", ScalarType.string) :: Nil, Nil)
              )
            )
          ).tree
        )
      )
    )

    raw.op should be(
      OnTag(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string)))
    )
//    raw.firstExecuted.map(_.tag) should be(
//      Some(
//        callService(LiteralRaw.quote("calledOutside"), "fn", Call(Nil, Nil)).tree.head
//      )
//    )
//    raw.lastExecuted.map(_.tag) should be(
//      Some(
//        callService(
//          LiteralRaw.quote("return"),
//          "fn",
//          Call(VarRaw("export", ScalarType.string) :: Nil, Nil)
//        ).tree.head
//      )
//    )
//    raw.lastExecuted.flatMap(_.seqPrev).flatMap(_.lastExecuted).map(_.tag) should be(
//      Some(
//        callService(
//          LiteralRaw.quote("calledInside"),
//          "fn",
//          Call(Nil, Call.Export("export", ScalarType.string) :: Nil)
//        ).tree.head
//      )
//    )

  }

  "raw cursor" should "move properly with fold" in {

    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            ValueRaw.InitPeerId,
            Chain.one(VarRaw("-relay-", ScalarType.string)),
            seq(
              callService(LiteralRaw.quote("calledOutside"), "fn", Call(Nil, Nil)),
              onVia(
                VarRaw("-other-", ScalarType.string),
                Chain.one(VarRaw("-external-", ScalarType.string)),
                fold(
                  "item",
                  VarRaw("iterable", ArrayType(ScalarType.string)),
                  onVia(
                    VarRaw("-in-fold-", ScalarType.string),
                    Chain.one(VarRaw("-fold-relay-", ScalarType.string)),
                    callService(
                      LiteralRaw.quote("calledInside"),
                      "fn",
                      Call(Nil, Call.Export("export", ScalarType.string) :: Nil)
                    )
                  )
                )
              ),
              callService(
                LiteralRaw.quote("return"),
                "fn",
                Call(VarRaw("export", ScalarType.string) :: Nil, Nil)
              )
            )
          ).tree
        )
      )
    )

    raw.op should be(
      OnTag(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string)))
    )
//    raw.firstExecuted.map(_.tag) should be(
//      Some(
//        callService(LiteralRaw.quote("calledOutside"), "fn", Call(Nil, Nil)).tree.head
//      )
//    )
//    raw.lastExecuted.map(_.tag) should be(
//      Some(
//        callService(
//          LiteralRaw.quote("return"),
//          "fn",
//          Call(VarRaw("export", ScalarType.string) :: Nil, Nil)
//        ).tree.head
//      )
//    )
//    raw.lastExecuted.flatMap(_.seqPrev).flatMap(_.lastExecuted).map(_.tag) should be(
//      Some(
//        callService(
//          LiteralRaw.quote("calledInside"),
//          "fn",
//          Call(Nil, Call.Export("export", ScalarType.string) :: Nil)
//        ).tree.head
//      )
//    )
//    raw.lastExecuted.flatMap(_.seqPrev).map(_.topology.pathOn).get should be(
//      OnTag(
//        VarRaw("-in-fold-", ScalarType.string),
//        Chain.one(VarRaw("-fold-relay-", ScalarType.string))
//      ) :: OnTag(
//        VarRaw("-other-", ScalarType.string),
//        Chain.one(VarRaw("-external-", ScalarType.string))
//      ) :: OnTag(
//        ValueRaw.InitPeerId,
//        Chain.one(VarRaw("-relay-", ScalarType.string))
//      ) :: Nil
//    )
//    raw.lastExecuted.map(_.topology.pathBefore).get should be(
//      Chain(
//        VarRaw("-fold-relay-", ScalarType.string),
//        VarRaw("-external-", ScalarType.string),
//        VarRaw("-relay-", ScalarType.string)
//      )
//    )

  }
}
