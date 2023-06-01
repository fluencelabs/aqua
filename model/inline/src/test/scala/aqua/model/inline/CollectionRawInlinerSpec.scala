package aqua.model.inline

import aqua.model.*
import aqua.model.inline.raw.{ApplyIntoCopyRawInliner, CollectionRawInliner}
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.{CollectionRaw, LiteralRaw, MakeStructRaw, VarRaw}
import aqua.types.{CanonStreamType, OptionType, ScalarType, StreamType, StructType}
import cats.data.{NonEmptyList, NonEmptyMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionRawInlinerSpec extends AnyFlatSpec with Matchers {

  "collection inliner" should "unfold struct with nested options (bug LNG-160)" in {

    val nestedType = StructType(
      "nested_type",
      NonEmptyMap.of("field1" -> ScalarType.u32)
    )
    val structType = StructType(
      "struct_type",
      NonEmptyMap.of("nestedOpt" -> OptionType(nestedType))
    )

    val makeStruct =
      MakeStructRaw(NonEmptyMap.of("field1" -> LiteralRaw.number(3)), nestedType)

    val value = VarRaw("l", ScalarType.string)
    val raw = CollectionRaw(NonEmptyList.of(makeStruct), OptionType(nestedType))

    val (v, tree) =
      RawValueInliner.valueToModel[InliningState](raw, false).run(InliningState()).value._2

    println(tree)

    tree.get.equalsOrShowDiff(
      RestrictionModel("option-inline", true).wrap(
        SeqModel.wrap(
          XorModel.wrap(
            SeqModel.wrap(
              CallServiceModel(
                "json",
                "obj",
                LiteralModel.fromRaw(LiteralRaw.quote("field1")) :: LiteralModel.fromRaw(
                  LiteralRaw.number(3)
                ) :: Nil,
                VarModel("nested_type_obj", nestedType)
              ).leaf,
              PushToStreamModel(
                VarModel("nested_type_obj", nestedType),
                CallModel.Export("option-inline", StreamType(nestedType))
              ).leaf
            ),
            NullModel.leaf
          ),
          CanonicalizeModel(
            VarModel("option-inline", StreamType(nestedType)),
            CallModel.Export("option-inline-0", CanonStreamType(nestedType))
          ).leaf
        )
      )
    ) shouldBe true

  }

}
