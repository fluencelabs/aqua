package aqua.model.inline

import aqua.model.inline.raw.ApplyPropertiesRawInliner
import aqua.model.{FlattenModel, FunctorModel, IntoFieldModel, IntoIndexModel, ParModel, SeqModel, ValueModel, VarModel}
import aqua.model.inline.state.InliningState
import aqua.raw.value.{ApplyPropertyRaw, FunctorRaw, IntoIndexRaw, LiteralRaw, VarRaw}
import aqua.types.*
import cats.data.NonEmptyMap
import cats.data.Chain
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class RawValueInlinerSpec extends AnyFlatSpec with Matchers {

  import RawValueInliner.valueToModel

  private def ysVarRaw(into: Int, name: String = "ys") =
    VarRaw(name, ArrayType(ScalarType.i8)).withProperty(
      IntoIndexRaw(LiteralRaw.number(into), ScalarType.i8)
    )

  private val `raw x[y]` = VarRaw("x", ArrayType(ScalarType.string)).withProperty(
    IntoIndexRaw(
      VarRaw("y", ScalarType.i8),
      ScalarType.string
    )
  )

  private val bType =
    StructType("objectType", NonEmptyMap(("c", ScalarType.string), SortedMap.empty))

  private val aType = StructType(
    "objectType",
    NonEmptyMap(
      ("b", bType),
      SortedMap.empty
    )
  )

  private val `raw res.c` = VarRaw(
    "res",
    bType
  ).withProperty(
    FunctorRaw(
      "c",
      ScalarType.string
    )
  )

  private val `raw ys[0]` = IntoIndexRaw(
    ysVarRaw(0),
    ScalarType.string
  )

  private val `raw ys[xyz!]` = IntoIndexRaw(
    ysVarRaw(0, "xyz"),
    ScalarType.string
  )

  private val `raw x[ys[0]]` = VarRaw("x", ArrayType(ScalarType.string)).withProperty(`raw ys[0]`)

  private val `raw x[ys[0]][ys[1]]` =
    VarRaw("x", ArrayType(ArrayType(ScalarType.string))).withProperty(
      IntoIndexRaw(ysVarRaw(0), ArrayType(ScalarType.string)),
      IntoIndexRaw(ysVarRaw(1), ScalarType.string)
    )

  private val `raw x[zs[ys[0]]][ys[1]]` =
    VarRaw("x", ArrayType(ArrayType(ScalarType.string))).withProperty(
      IntoIndexRaw(
        VarRaw("zs", ArrayType(ScalarType.i8)).withProperty(
          IntoIndexRaw(
            ysVarRaw(0),
            ScalarType.i8
          )
        ),
        ArrayType(ScalarType.string)
      ),
      IntoIndexRaw(ysVarRaw(1), ScalarType.string)
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

  // TODO: unignore and fix after stream restrictions will be implemented
  ignore /*"raw value inliner"*/ should "unfold an IntoField PropertyModel" in {
    import aqua.model.inline.state.Mangler.Simple
    // a.field1.field2
    valueToModel[InliningState](`raw res.c`)
      .run(
        InliningState(resolvedExports =
          Map("res" -> VarModel("a", aType, Chain.one(IntoFieldModel("b", bType))))
        )
      )
      .value
      ._2 should be(
      VarModel(
        "a",
        aType,
        Chain(IntoFieldModel("b", bType), IntoFieldModel("c", ScalarType.string))
      ) -> None
    )
  }

  "raw value inliner" should "unfold a PropertyModel" in {
    import aqua.model.inline.state.Mangler.Simple
    // [ys!]
    ApplyPropertiesRawInliner
      .unfoldProperty[InliningState](`raw ys[0]`)
      .run(InliningState(noNames = Set("ys")))
      .value
      ._2 should be(
      IntoIndexModel("ap-prop", ScalarType.string) -> Inline(
        Map(
          "ap-prop" -> VarRaw("ys", ArrayType(ScalarType.i8)).withProperty(
            IntoIndexRaw(LiteralRaw.number(0), ScalarType.i8)
          )
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
        Chain.one(IntoIndexModel("ap-prop", ScalarType.string))
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
        "ap-prop"
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
          IntoIndexModel("ap-prop", ArrayType(ScalarType.string)),
          IntoIndexModel("ap-prop-0", ScalarType.string)
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
          "ap-prop"
        ).leaf,
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", ScalarType.i8))
          ),
          "ap-prop-0"
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
          IntoIndexModel("ap-prop", ArrayType(ScalarType.string)),
          IntoIndexModel("ap-prop-0", ScalarType.string)
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
            "ap-prop-1"
          ).leaf,
          // Then use that ys-1 as an index of zs
          FlattenModel(
            VarModel(
              "zs",
              ArrayType(ScalarType.i8),
              Chain.one(IntoIndexModel("ap-prop-1", ScalarType.i8))
            ),
            "ap-prop"
          ).leaf
        ),
        // Now prepare ys-0
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", ScalarType.i8))
          ),
          "ap-prop-0"
        ).leaf
      )
    ) should be(true)
  }

}
