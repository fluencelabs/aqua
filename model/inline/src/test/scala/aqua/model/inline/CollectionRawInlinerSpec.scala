package aqua.model.inline

import aqua.model.*
import aqua.model.inline.raw.{ApplyIntoCopyRawInliner, CollectionRawInliner}
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.{CollectionRaw, LiteralRaw, MakeStructRaw, VarRaw}
import aqua.types.{CanonStreamType, OptionType, ScalarType, StreamMapType, StreamType, StructType}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionRawInlinerSpec extends AnyFlatSpec with Matchers {

  "collection inliner" should "unfold struct with nested options (bug LNG-160)" in {

    val nestedType = StructType(
      "nested_type",
      NonEmptyMap.of("field1" -> ScalarType.u32)
    )

    val makeStruct =
      MakeStructRaw(NonEmptyMap.of("field1" -> LiteralRaw.number(3)), nestedType)

    val raw = CollectionRaw(NonEmptyList.of(makeStruct), OptionType(nestedType))

    val (v, tree) =
      RawValueInliner.valueToModel[InliningState](raw, false).runA(InliningState()).value

    val resultValue = VarModel("option-inline-0", CanonStreamType(nestedType))

    v shouldBe resultValue

    val streamMapType = StreamMapType.fromStruct(nestedType)
    val streamMapVar = VarModel("nested_type_obj_map", streamMapType)

    val expected =
      RestrictionModel("option-inline", StreamType(nestedType)).wrap( // create a stream
        SeqModel.wrap(
          // create an object
          SeqModel.wrap(
            InsertKeyValueModel(LiteralModel.quote("field1"), LiteralModel.number(3), streamMapVar.name, streamMapType).leaf,
            CanonicalizeModel(streamMapVar, CallModel.Export("nested_type_obj", nestedType)).leaf
          ),
          XorModel.wrap(
            SeqModel.wrap(
              // push object to the stream
              PushToStreamModel(
                VarModel("nested_type_obj", nestedType),
                CallModel.Export("option-inline", StreamType(nestedType))
              ).leaf
            ),
            NullModel.leaf
          ),
          // canonicalize the stream to use it after inlining
          CanonicalizeModel(
            VarModel("option-inline", StreamType(nestedType)),
            CallModel.Export(resultValue.name, resultValue.baseType)
          ).leaf
        )
      )

    tree.get.equalsOrShowDiff(expected) shouldBe true
  }

}
