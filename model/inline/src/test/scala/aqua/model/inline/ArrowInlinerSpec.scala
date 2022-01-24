package aqua.model.inline

import aqua.model.{CallModel, CallServiceModel, FuncArrow, LiteralModel, OpModel, SeqModel}
import aqua.model.inline.state.InliningState
import aqua.raw.ops.{
  Call,
  CallArrowTag,
  CallServiceTag,
  DeclareStreamTag,
  RawTag,
  ReturnTag,
  SeqTag
}
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.types.{
  ArrayType,
  ArrowType,
  LabelledConsType,
  LiteralType,
  NilType,
  ProductType,
  ScalarType,
  StreamType
}
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowInlinerSpec extends AnyFlatSpec with Matchers {

  "arrow inliner" should "convert simple arrow" in {

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          CallServiceTag(LiteralRaw.quote("dumb_srv_id"), "dumb", Call(Nil, Nil)).leaf,
          ArrowType(ProductType(Nil), ProductType(Nil)),
          Nil,
          Map.empty,
          Map.empty
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      CallServiceModel(
        LiteralModel("\"dumb_srv_id\"", LiteralType.string),
        "dumb",
        CallModel(Nil, Nil)
      ).leaf
    ) should be(true)

  }

  "arrow inliner" should "work with streams as arguments" ignore {

    val returnType = ArrayType(ArrayType(ScalarType.string))
    val streamType = StreamType(ArrayType(ScalarType.string))
    val recordsVar = VarRaw("records", streamType)
    val innerRecordsVar = VarRaw("inner-records", StreamType(ArrayType(ScalarType.string)))
    val innerName = "inner"

    val inner = FuncArrow(
      innerName,
      CallServiceTag(
        LiteralRaw.quote("test-service"),
        "get_records",
        Call(Nil, Call.Export(innerRecordsVar.name, streamType) :: Nil)
      ).leaf,
      ArrowType(
        ProductType(LabelledConsType(innerRecordsVar.name, streamType, NilType) :: Nil),
        ProductType(Nil)
      ),
      Nil,
      Map.empty,
      Map.empty
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "outer",
          SeqTag.wrap(
            DeclareStreamTag(recordsVar).leaf,
            CallArrowTag(innerName, Call(recordsVar :: Nil, Nil)).leaf,
            ReturnTag(NonEmptyList.one(recordsVar)).leaf
          ),
          ArrowType(ProductType(Nil), ProductType(returnType :: Nil)),
          Nil,
          Map(innerName -> inner),
          Map.empty
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      CallServiceModel(
        LiteralModel("\"dumb_srv_id\"", LiteralType.string),
        "dumb",
        CallModel(Nil, Nil)
      ).leaf
    ) should be(true)

  }

}
