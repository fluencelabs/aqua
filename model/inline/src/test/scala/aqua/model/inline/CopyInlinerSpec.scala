package aqua.model.inline

import aqua.model.*
import aqua.model.inline.raw.ApplyIntoCopyRawInliner
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.*
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CopyInlinerSpec extends AnyFlatSpec with Matchers {

  "copy inliner" should "unfold values in parallel" in {

    val structType = StructType(
      "struct_type",
      NonEmptyMap.of("field1" -> ScalarType.u32, "field2" -> ScalarType.string)
    )

    val arrType = ArrayType(ScalarType.string)
    val length = FunctorRaw("length", ScalarType.u32)
    val lengthValue = VarRaw("l", arrType).withProperty(length)

    val getField = CallArrowRaw(None, "get_field", Nil, ArrowType(NilType, UnlabeledConsType(ScalarType.string, NilType)), Option(LiteralRaw("\"serv\"", ScalarType.string)))

    val copyRaw =
      IntoCopyRaw(structType, NonEmptyMap.of("field1" -> lengthValue, "field2" -> getField))
    val varName = "some_struct"
    val varRaw = VarRaw(varName, structType).withProperty(copyRaw)

    val (model, tree) =
      RawValueInliner.valueToModel[InliningState](varRaw, false).run(InliningState()).value._2

    val result = VarModel(varName + "_obj_copy", structType)
    model shouldBe result

    val lengthModel = FunctorModel("length", ScalarType.u32)

    tree.get.equalsOrShowDiff(
      SeqModel.wrap(
        ParModel.wrap(
          SeqModel.wrap(
            FlattenModel(VarModel("l", arrType), "l_to_functor").leaf,
            FlattenModel(VarModel("l_to_functor", arrType, Chain.one(lengthModel)), "l_length").leaf,
          ),
          CallServiceModel(
            "serv",
            "get_field",
            Nil,
            VarModel("get_field", ScalarType.string)
          ).leaf
        ),
        CallServiceModel(
          "json",
          "puts",
          VarModel(varName, structType) :: LiteralModel.fromRaw(
            LiteralRaw.quote("field1")
          ) :: VarModel("l_length", ScalarType.u32) :: LiteralModel.fromRaw(
            LiteralRaw.quote("field2")
          ) :: VarModel("get_field", ScalarType.string) :: Nil,
          result
        ).leaf
      )
    ) shouldBe true

  }

}