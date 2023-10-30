package aqua.model.transform.topology

import aqua.model.transform.ModelBuilder
import aqua.model.{CallModel, ForModel, OnModel, SeqModel}
import aqua.model.transform.cursor.ChainZipper
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.raw.ops.{Call, FuncOp, OnTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, ScalarType}

import cats.data.{Chain, NonEmptyList}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpModelTreeCursorSpec extends AnyFlatSpec with Matchers {

  import ModelBuilder.*

  "simple raw cursor on init_peer_id" should "move properly" in {
    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          OnModel(ValueRaw.InitPeerId, Chain.empty).wrap(
            callModel(1, Nil)
          )
        )
      )
    )

    // raw.firstExecuted shouldBe raw.lastExecuted
  }

  "simple raw cursor with multiple calls" should "move on seqs" in {
    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          OnModel(ValueRaw.InitPeerId, Chain.empty).wrap(
            SeqModel.wrap(
              callModel(1),
              callModel(2),
              callModel(3),
              callModel(4)
            )
          )
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
          OnModel(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string))).wrap(
            callModel(1)
          )
        )
      )
    )

    // raw.firstExecuted shouldBe raw.lastExecuted
  }

  "raw cursor" should "move properly" in {

    val raw = OpModelTreeCursor(
      NonEmptyList.one(
        ChainZipper.one(
          OnModel(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string))).wrap(
            SeqModel.wrap(
              callModel(1),
              OnModel(
                VarRaw("-other-", ScalarType.string),
                Chain.one(VarRaw("-external-", ScalarType.string))
              ).wrap(
                callModel(
                  2,
                  CallModel.Export("export", ScalarType.string) :: Nil
                )
              ),
              callModel(
                3,
                Nil
              )
            )
          )
        )
      )
    )

    raw.op should be(
      OnModel(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string)))
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
          OnModel(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string))).wrap(
            SeqModel.wrap(
              callModel(1),
              OnModel(
                VarRaw("-other-", ScalarType.string),
                Chain.one(VarRaw("-external-", ScalarType.string))
              ).wrap(
                fold(
                  "item",
                  VarRaw("iterable", ArrayType(ScalarType.string)),
                  ForModel.Mode.Null,
                  OnModel(
                    VarRaw("-in-fold-", ScalarType.string),
                    Chain.one(VarRaw("-fold-relay-", ScalarType.string))
                  ).wrap(
                    callModel(
                      2,
                      CallModel.Export("export", ScalarType.string) :: Nil
                    )
                  )
                )
              ),
              callModel(
                3
              )
            )
          )
        )
      )
    )

    raw.op should be(
      OnModel(ValueRaw.InitPeerId, Chain.one(VarRaw("-relay-", ScalarType.string)))
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
