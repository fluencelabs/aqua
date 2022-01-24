package aqua.model.inline

import aqua.model.{FlattenModel, IntoIndexModel, ParModel, SeqModel, VarModel}
import aqua.model.inline.state.InliningState
import aqua.raw.value.{IntoIndexRaw, LiteralRaw, VarRaw}
import aqua.types.*
import cats.data.NonEmptyMap
import cats.data.Chain
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RawValueInlinerSpec extends AnyFlatSpec with Matchers {

  import RawValueInliner.{unfoldLambda, valueToModel}

  private def ysVarRaw(into: Int, name: String = "ys"): VarRaw = VarRaw(
    name,
    ArrayType(ScalarType.i8),
    Chain.one(IntoIndexRaw(LiteralRaw.number(into), ScalarType.i8))
  )

  private val `raw x[y]` = VarRaw(
    "x",
    ArrayType(ScalarType.string),
    Chain.one(
      IntoIndexRaw(
        VarRaw("y", ScalarType.i8),
        ScalarType.string
      )
    )
  )

  private val `raw ys[0]` = IntoIndexRaw(
    ysVarRaw(0),
    ScalarType.string
  )

  private val `raw x[ys[0]]` = VarRaw("x", ArrayType(ScalarType.string), Chain.one(`raw ys[0]`))

  private val `raw x[ys[0]][ys[1]]` =
    VarRaw(
      "x",
      ArrayType(ArrayType(ScalarType.string)),
      Chain(
        IntoIndexRaw(ysVarRaw(0), ArrayType(ScalarType.string)),
        IntoIndexRaw(ysVarRaw(1), ScalarType.string)
      )
    )

  private val `raw x[zs[ys[0]]][ys[1]]` =
    VarRaw(
      "x",
      ArrayType(ArrayType(ScalarType.string)),
      Chain(
        IntoIndexRaw(
          VarRaw(
            "zs",
            ArrayType(ScalarType.i8),
            Chain.one(
              IntoIndexRaw(
                ysVarRaw(0),
                ScalarType.i8
              )
            )
          ),
          ArrayType(ScalarType.string)
        ),
        IntoIndexRaw(ysVarRaw(1), ScalarType.string)
      )
    )

  "raw value inliner" should "desugarize a single non-recursive raw value" in {
    // x[y]
    valueToModel[InliningState](`raw x[y]`)
      .run(InliningState(noNames = Set("x", "y")))
      .value
      ._2 should be(
      VarModel(
        "x",
        ArrayType(ScalarType.string),
        Chain.one(IntoIndexModel("y", ScalarType.string))
      ) -> None
    )
  }

  "raw value inliner" should "unfold a LambdaModel" in {
    import aqua.model.inline.state.Mangler.Simple
    // [ys!]
    unfoldLambda[Set[String]](`raw ys[0]`)
      .run(Set("ys"))
      .value
      ._2 should be(
      IntoIndexModel("ys-0", ScalarType.string) -> Map(
        "ys-0" -> VarRaw(
          "ys",
          ArrayType(ScalarType.i8),
          Chain.one(IntoIndexRaw(LiteralRaw.number(0), ScalarType.i8))
        )
      )
    )
  }

  "raw value inliner" should "desugarize a single recursive raw value" in {
    // x[ys!]
    val (resVal, resTree) = valueToModel[InliningState](
      `raw x[ys[0]]`
    )
      .run(InliningState(noNames = Set("x", "ys")))
      .value
      ._2

    resVal should be(
      VarModel(
        "x",
        ArrayType(ScalarType.string),
        Chain.one(IntoIndexModel("ys-0", ScalarType.string))
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      FlattenModel(
        VarModel(
          "ys",
          ArrayType(ScalarType.i8),
          Chain.one(IntoIndexModel("0", ScalarType.i8))
        ),
        "ys-0"
      ).leaf
    ) should be(true)
  }

  "raw value inliner" should "desugarize x[ys[0]][ys[1]] and make proper flattener tags" in {
    val (resVal, resTree) = valueToModel[InliningState](
      `raw x[ys[0]][ys[1]]`
    )
      .run(InliningState(noNames = Set("x", "ys")))
      .value
      ._2

    resVal should be(
      VarModel(
        "x",
        ArrayType(ArrayType(ScalarType.string)),
        Chain(
          IntoIndexModel("ys-0", ArrayType(ScalarType.string)),
          IntoIndexModel("ys-1", ScalarType.string)
        )
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      ParModel.wrap(
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("0", ScalarType.i8))
          ),
          "ys-0"
        ).leaf,
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", ScalarType.i8))
          ),
          "ys-1"
        ).leaf
      )
    ) should be(true)
  }

  "raw value inliner" should "desugarize a recursive lambda value" in {
    val (resVal, resTree) = valueToModel[InliningState](
      `raw x[zs[ys[0]]][ys[1]]`
    )
      .run(InliningState(noNames = Set("x", "ys", "zs")))
      .value
      ._2

    // This is x[zs-0][ys-0]
    // zs-0 should be zs[ys[0]], which should be already flattened
    resVal should be(
      VarModel(
        "x",
        ArrayType(ArrayType(ScalarType.string)),
        Chain(
          IntoIndexModel("zs-0", ArrayType(ScalarType.string)),
          IntoIndexModel("ys-0", ScalarType.string)
        )
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      ParModel.wrap(
        // Prepare the zs-0 index
        SeqModel.wrap(
          // First get ys[0], save as ys-1
          FlattenModel(
            VarModel(
              "ys",
              ArrayType(ScalarType.i8),
              Chain.one(IntoIndexModel("0", ScalarType.i8))
            ),
            "ys-1"
          ).leaf,
          // Then use that ys-1 as an index of zs
          FlattenModel(
            VarModel(
              "zs",
              ArrayType(ScalarType.i8),
              Chain.one(IntoIndexModel("ys-1", ScalarType.i8))
            ),
            "zs-0"
          ).leaf
        ),
        // Now prepare ys-0
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", ScalarType.i8))
          ),
          "ys-0"
        ).leaf
      )
    ) should be(true)
  }

}
