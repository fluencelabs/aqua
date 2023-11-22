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

class MakeStructInlinerSpec extends AnyFlatSpec with Matchers {

  "make struct inliner" should "unfold values in parallel" in {

    val structType = StructType(
      "struct_type",
      NonEmptyMap.of("field1" -> ScalarType.u32, "field2" -> ScalarType.string)
    )

    val arrType = ArrayType(ScalarType.string)
    val length = FunctorRaw("length", ScalarType.u32)
    val lengthValue = VarRaw("l", arrType).withProperty(length)

    val getField = CallServiceRaw(
      LiteralRaw.quote("serv"),
      "get_field",
      ArrowType(NilType, UnlabeledConsType(ScalarType.string, NilType)),
      Nil
    )

    val makeStruct =
      MakeStructRaw(NonEmptyMap.of("field1" -> lengthValue, "field2" -> getField), structType)
    val varName = structType.name

    val (model, tree) =
      RawValueInliner.valueToModel[InliningState](makeStruct, false).run(InliningState()).value._2

    val result = VarModel(varName + "_obj", structType)
    model shouldBe result

    val lengthModel = FunctorModel("length", ScalarType.u32)

    val streamMapName = "struct_type_obj_map"
    val streamMapType = StreamMapType.top()

    tree.get.equalsOrShowDiff(
      SeqModel.wrap(
        ParModel.wrap(
          SeqModel.wrap(
            FlattenModel(VarModel("l", arrType), "l_to_functor").leaf,
            FlattenModel(VarModel("l_to_functor", arrType, Chain.one(lengthModel)), "l_length").leaf
          ),
          CallServiceModel(
            "serv",
            "get_field",
            Nil,
            VarModel("get_field", ScalarType.string)
          ).leaf
        ),
        RestrictionModel(streamMapName, streamMapType).wrap(
          InsertKeyValueModel(
            LiteralModel.quote("field1"),
            VarModel("l_length", ScalarType.u32),
            streamMapName,
            streamMapType
          ).leaf,
          InsertKeyValueModel(
            LiteralModel.quote("field2"),
            VarModel("get_field", ScalarType.string),
            streamMapName,
            streamMapType
          ).leaf,
          CanonicalizeModel(
            VarModel(streamMapName, streamMapType),
            CallModel.Export(result.name, result.`type`)
          ).leaf
        )
      )
    ) shouldBe true

  }

}
