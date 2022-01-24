package aqua.model.inline

import aqua.model.*
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.types.*
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

  /*
  func stream-callback(cb: []string -> ()):
	records: *string
	cb(records)
   */
  "arrow inliner" should "pass stream to callback properly" in {

    val streamType = StreamType(ScalarType.string)
    val streamVar = VarRaw("records", streamType)
    val arrayModel = VarModel("records", ArrayType(ScalarType.string))
    val cbType = ArrowType(ProductType(ArrayType(ScalarType.string) :: Nil), ProductType(Nil))
    val cbVal = VarModel("cb-pass", cbType)

    val cbArg = VarRaw("cbVar", ArrayType(ScalarType.string))

    val cbArrow = FuncArrow(
      "cb",
      SeqTag.wrap(
        CallServiceTag(
          LiteralRaw.quote("test-service"),
          "some-call",
          Call(cbArg :: Nil, Nil)
        ).leaf
      ),
      ArrowType(
        ProductType.labelled(
          (
            cbArg.name,
            cbArg.`type`
          ) :: Nil
        ),
        ProductType(Nil)
      ),
      Nil,
      Map.empty,
      Map.empty
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "stream-callback",
          SeqTag.wrap(
            DeclareStreamTag(streamVar).leaf,
            CallArrowTag("cb", Call(streamVar :: Nil, Nil)).leaf
          ),
          ArrowType(
            ProductType.labelled(
              (
                "cb",
                cbType
              ) :: Nil
            ),
            ProductType(Nil)
          ),
          Nil,
          Map("cb" -> cbArrow),
          Map.empty
        ),
        CallModel(cbVal :: Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrapWithEmpty(
        EmptyModel.leaf,
        CallServiceModel(
          LiteralModel("\"test-service\"", LiteralType.string),
          "some-call",
          CallModel(arrayModel :: Nil, Nil)
        ).leaf
      )
    ) should be(true)

  }

  /*
  service TestService("test-service"):
    get_records() -> []string

  func inner(inner-records: *[]string):
    inner-records <- TestService.get_records()

  func retrieve_records() -> [][]string:
      records: *[]string
      -- 'inner-records' argument in `inner` should be renamed as `records` in resulted AIR
      append_records(records)
      <- records
   */
  "arrow inliner" should "work with streams as arguments" in {

    val returnType = ArrayType(ArrayType(ScalarType.string))
    val streamType = StreamType(ArrayType(ScalarType.string))
    val recordsVar = VarRaw("records", streamType)
    val recordsModel = VarModel(recordsVar.name, recordsVar.baseType)
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
        ProductType.labelled((innerRecordsVar.name -> streamType) :: Nil),
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
            CallServiceTag(
              LiteralRaw.quote("callbackSrv"),
              "response",
              Call(recordsVar :: Nil, Nil)
            ).leaf
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
      SeqModel.wrapWithEmpty(
        EmptyModel.leaf,
        CallServiceModel(
          LiteralModel("\"test-service\"", LiteralType.string),
          "get_records",
          CallModel(Nil, CallModel.Export(recordsModel.name, recordsModel.`type`) :: Nil)
        ).leaf,
        CallServiceModel(
          LiteralModel("\"callbackSrv\"", LiteralType.string),
          "response",
          CallModel(recordsModel :: Nil, Nil)
        ).leaf
      )
    ) should be(true)

  }

}
