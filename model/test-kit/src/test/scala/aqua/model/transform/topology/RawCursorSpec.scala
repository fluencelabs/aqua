package aqua.model.transform.topology

import aqua.model.func.Call
import aqua.model.func.raw.{FuncOp, FuncOps, OnTag, ReturnTag}
import aqua.model.transform.cursor.ChainZipper
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.ScalarType
import cats.data.{Chain, NonEmptyList}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RawCursorSpec extends AnyFlatSpec with Matchers {
  import FuncOp.*
  import FuncOps.*

  "simple raw cursor on init_peer_id" should "move properly" in {
    val raw = RawCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            LiteralModel.initPeerId,
            Chain.empty,
            callService(LiteralModel.quote("calledOutside"), "fn", Call(Nil, Nil)),
          ).tree
        )
      )
    )

    raw.firstExecuted shouldBe raw.lastExecuted
  }

  "simple raw cursor with multiple calls" should "move on seqs" in {
    val raw = RawCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            LiteralModel.initPeerId,
            Chain.empty,
            seq(
              callService(LiteralModel.quote("1"), "fn", Call(Nil, Nil)),
              callService(LiteralModel.quote("2"), "fn", Call(Nil, Nil)),
              callService(LiteralModel.quote("3"), "fn", Call(Nil, Nil)),
              callService(LiteralModel.quote("4"), "fn", Call(Nil, Nil)),
            )

          ).tree
        )
      )
    )

    raw.lastExecuted shouldBe raw.firstExecuted.get.seqNext.get.seqNext.get.seqNext
    raw.lastExecuted.get.seqPrev shouldBe raw.firstExecuted.get.seqNext.get.seqNext
    raw.lastExecuted.get.seqPrev.get.seqPrev shouldBe raw.firstExecuted.get.seqNext
    raw.lastExecuted.get.seqPrev shouldBe raw.firstExecuted.get.seqNext.get.seqNext
  }

  "simple raw cursor on init_peer_id via relay" should "move properly" in {
    val raw = RawCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            LiteralModel.initPeerId,
            Chain.one(VarModel("-relay-", ScalarType.string)),
            callService(LiteralModel.quote("calledOutside"), "fn", Call(Nil, Nil)),
          ).tree
        )
      )
    )

    raw.firstExecuted shouldBe raw.lastExecuted
  }

  "raw cursor" should "move properly" in {

    val raw = RawCursor(
      NonEmptyList.one(
        ChainZipper.one(
          onVia(
            LiteralModel.initPeerId,
            Chain.one(VarModel("-relay-", ScalarType.string)),
            seq(
              callService(LiteralModel.quote("calledOutside"), "fn", Call(Nil, Nil)),
              onVia(
                VarModel("-other-", ScalarType.string),
                Chain.one(VarModel("-external-", ScalarType.string)),
                seq(
                  callService(
                    LiteralModel.quote("calledInside"),
                    "fn",
                    Call(Nil, Call.Export("export", ScalarType.string) :: Nil)
                  ),
                  leaf(ReturnTag(NonEmptyList.one(VarModel("export", ScalarType.string))))
                )
              ),
              callService(
                LiteralModel.quote("return"),
                "fn",
                Call(VarModel("export", ScalarType.string) :: Nil, Nil)
              )
            )
          ).tree
        )
      )
    )

    raw.tag should be(
      OnTag(LiteralModel.initPeerId, Chain.one(VarModel("-relay-", ScalarType.string)))
    )
    raw.firstExecuted.map(_.tag) should be(
      Some(
        callService(LiteralModel.quote("calledOutside"), "fn", Call(Nil, Nil)).tree.head
      )
    )
    raw.lastExecuted.map(_.tag) should be(
      Some(
        callService(
          LiteralModel.quote("return"),
          "fn",
          Call(VarModel("export", ScalarType.string) :: Nil, Nil)
        ).tree.head
      )
    )
    raw.lastExecuted.flatMap(_.seqPrev).flatMap(_.lastExecuted).map(_.tag) should be(
      Some(
        callService(
          LiteralModel.quote("calledInside"),
          "fn",
          Call(Nil, Call.Export("export", ScalarType.string) :: Nil)
        ).tree.head
      )
    )

  }

}
