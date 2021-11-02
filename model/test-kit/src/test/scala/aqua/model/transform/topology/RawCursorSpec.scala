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
