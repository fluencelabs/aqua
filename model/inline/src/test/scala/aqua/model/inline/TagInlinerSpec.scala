package aqua.model.inline

import aqua.model.{IntoIndexModel, VarModel}
import aqua.model.inline.state.InliningState
import aqua.raw.value.{IntoIndexRaw, VarRaw}
import aqua.types.*
import cats.data.NonEmptyMap
import cats.data.Chain
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TagInlinerSpec extends AnyFlatSpec with Matchers {

  "tag inliner" should "desugarize a single non-recursive raw value" in {
    TagInliner
      .desugarize[InliningState](
        VarRaw(
          "x",
          ArrayType(ScalarType.string),
          Chain.one(
            IntoIndexRaw(
              VarRaw("y", ScalarType.i8),
              ScalarType.string
            )
          )
        )
      )
      .run(InliningState())
      .value
      ._2 should be(
      VarModel(
        "x",
        ArrayType(ScalarType.string),
        Chain.one(IntoIndexModel("y", ScalarType.string))
      ) -> None
    )
  }

}
