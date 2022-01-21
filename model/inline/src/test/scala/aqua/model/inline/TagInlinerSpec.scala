package aqua.model.inline

import aqua.model.{FlattenModel, IntoIndexModel, VarModel}
import aqua.model.inline.state.InliningState
import aqua.raw.value.{IntoIndexRaw, LiteralRaw, VarRaw}
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

  "tag inliner" should "desugarize a single recursive raw value" in {
    TagInliner
      .desugarize[InliningState](
        VarRaw(
          "x",
          ArrayType(ScalarType.string),
          Chain.one(
            IntoIndexRaw(
              VarRaw(
                "ys",
                ArrayType(ScalarType.i8),
                Chain.one(IntoIndexRaw(LiteralRaw.number(0), LiteralType.number))
              ),
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
        Chain.one(IntoIndexModel("ys-1", ScalarType.string))
      ) -> Some(
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("0", LiteralType.number))
          ),
          "ys-1"
        ).leaf
      )
    )
  }

}
