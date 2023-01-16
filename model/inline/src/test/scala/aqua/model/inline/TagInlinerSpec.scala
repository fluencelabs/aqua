package aqua.model.inline

import aqua.model.{LiteralModel, OpModel, SeqModel}
import aqua.model.inline.state.InliningState
import aqua.raw.ops.{Call, CanonicalizeTag, FlattenTag}
import aqua.raw.value.ValueRaw
import aqua.types.{ScalarType, StreamType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show.*

class TagInlinerSpec extends AnyFlatSpec with Matchers {

  "CanonicalizeTag" should "pass literals as is" in {
    val canonTo = "canon_to"

    val model = TagInliner
      .tagToModel[InliningState](
        CanonicalizeTag(ValueRaw.Nil, Call.Export(canonTo, StreamType(ScalarType.string))),
        ""
      )
      .run(InliningState())
      .value

    model._1.resolvedExports(canonTo) shouldBe LiteralModel(ValueRaw.Nil.value, ValueRaw.Nil.baseType)
    model._2._1 shouldBe Some(SeqModel)
    model._2._2 shouldBe None
  }

  "FlattenTag" should "pass literals as is" in {
    val canonTo = "canon_to"

    val model = TagInliner
      .tagToModel[InliningState](
        FlattenTag(ValueRaw.Nil, canonTo),
        ""
      )
      .run(InliningState())
      .value

    model._1.resolvedExports(canonTo) shouldBe LiteralModel(
      ValueRaw.Nil.value,
      ValueRaw.Nil.baseType
    )
    model._2._1 shouldBe Some(SeqModel)
    model._2._2 shouldBe None
  }
}
