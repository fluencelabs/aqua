package aqua.model.inline

import aqua.model.{LiteralModel, OpModel, SeqModel}
import aqua.model.inline.state.InliningState
import aqua.raw.ops.{Call, CanonicalizeTag, FlattenTag}
import aqua.raw.value.ValueRaw
import aqua.types.{ScalarType, StreamType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show.*
import org.scalatest.Inside
import aqua.model.inline.TagInliner.TagInlined

class TagInlinerSpec extends AnyFlatSpec with Matchers with Inside {

  "CanonicalizeTag" should "pass literals as is" in {
    val canonTo = "canon_to"

    val (state, inlined) = TagInliner
      .tagToModel[InliningState](
        CanonicalizeTag(ValueRaw.Nil, Call.Export(canonTo, StreamType(ScalarType.string)))
      )
      .run(InliningState())
      .value

    state.resolvedExports(canonTo) shouldBe LiteralModel(
      ValueRaw.Nil.value,
      ValueRaw.Nil.baseType
    )

    inside(inlined) { case TagInlined.Empty(prefix) =>
      prefix shouldBe None
    }
  }

  "FlattenTag" should "pass literals as is" in {
    val canonTo = "canon_to"

    val (state, inlined) = TagInliner
      .tagToModel[InliningState](
        FlattenTag(ValueRaw.Nil, canonTo)
      )
      .run(InliningState())
      .value

    state.resolvedExports(canonTo) shouldBe LiteralModel(
      ValueRaw.Nil.value,
      ValueRaw.Nil.baseType
    )
    
    inside(inlined) { case TagInlined.Empty(prefix) =>
      prefix shouldBe None
    }
  }
}
