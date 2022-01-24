package aqua.model.inline

import aqua.model.{FlattenModel, IntoIndexModel, ParModel, VarModel}
import aqua.model.inline.state.InliningState
import aqua.raw.value.{IntoIndexRaw, LiteralRaw, VarRaw}
import aqua.types.*
import cats.data.NonEmptyMap
import cats.data.Chain
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TagInlinerSpec extends AnyFlatSpec with Matchers {

  private def ysVarRaw(into: Int): VarRaw = VarRaw(
    "ys",
    ArrayType(ScalarType.i8),
    Chain.one(IntoIndexRaw(LiteralRaw.number(0), LiteralType.number))
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

  "tag inliner" should "desugarize a single non-recursive raw value" in {
    // x[y]
    TagInliner
      .desugarize[InliningState](`raw x[y]`)
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

  "tag inliner" should "unfold a LambdaModel" in {
    import aqua.model.inline.state.Counter.Simple
    // [ys!]
    TagInliner
      .unfoldLambda[Int](`raw ys[0]`)
      .run(0)
      .value
      ._2 should be(
      IntoIndexModel("ys-1", ScalarType.string) -> Map(
        "ys-1" -> VarRaw(
          "ys",
          ArrayType(ScalarType.i8),
          Chain.one(IntoIndexRaw(LiteralRaw.number(0), LiteralType.number))
        )
      )
    )
  }

  "tag inliner" should "desugarize a single recursive raw value" in {
    // x[ys!]
    val (resVal, resTree) = TagInliner
      .desugarize[InliningState](
        `raw x[ys[0]]`
      )
      .run(InliningState())
      .value
      ._2

    resVal should be(
      VarModel(
        "x",
        ArrayType(ScalarType.string),
        Chain.one(IntoIndexModel("ys-1", ScalarType.string))
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      FlattenModel(
        VarModel(
          "ys",
          ArrayType(ScalarType.i8),
          Chain.one(IntoIndexModel("0", LiteralType.number))
        ),
        "ys-1"
      ).leaf
    ) should be(true)
  }

  "tag inliner" should "desugarize x[ys[0]][ys[1]] and make proper flattener tags" in {
    val (resVal, resTree) = TagInliner
      .desugarize[InliningState](
        `raw x[ys[0]][ys[1]]`
      )
      .run(InliningState())
      .value
      ._2

    resVal should be(
      VarModel(
        "x",
        ArrayType(ArrayType(ScalarType.string)),
        Chain(
          IntoIndexModel("ys-1", ArrayType(ScalarType.string)),
          IntoIndexModel("ys-2", ScalarType.string)
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
            Chain.one(IntoIndexModel("0", LiteralType.number))
          ),
          "ys-1"
        ).leaf,
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", LiteralType.number))
          ),
          "ys-2"
        ).leaf
      )
    ) should be(true)
  }

}
