package aqua.model.inline

import aqua.model.*
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.{IntoFieldRaw, LiteralRaw, VarRaw}
import aqua.types.*
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
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
    val streamModel = VarModel("records", StreamType(ScalarType.string))
    val cbType = ArrowType(ProductType(ArrayType(ScalarType.string) :: Nil), ProductType(Nil))
    val cbVal = VarModel("cb-pass", cbType)

    val cbArg = VarRaw("cbVar", ArrayType(ScalarType.string))

    val cbArrow = FuncArrow(
      "cb",
      CallServiceTag(
        LiteralRaw.quote("test-service"),
        "some-call",
        Call(cbArg :: Nil, Nil)
      ).leaf,
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
          RestrictionTag(streamVar.name, true).wrap(
            SeqTag.wrap(
              DeclareStreamTag(streamVar).leaf,
              CallArrowTag("cb", Call(streamVar :: Nil, Nil)).leaf
            )
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
      RestrictionModel(streamVar.name, true).wrap(
        SeqModel.wrapWithEmpty(
          EmptyModel.leaf,
          CallServiceModel(
            LiteralModel("\"test-service\"", LiteralType.string),
            "some-call",
            CallModel(streamModel :: Nil, Nil)
          ).leaf
        )
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

  /*
   data Prod:
     value: string

   service OpHa("op"):
     array(a: string, b: string) -> []string
     identity(a: string) -> string

   func doSmth(arg: Prod):
     v = arg.value
     OpHa.identity(v)
   */
  "arrow inliner" should "hold lambda" in {

    // lambda that will be assigned to another variable
    val objectVarLambda = VarRaw(
      "object",
      StructType("objectType", NonEmptyMap.one("field", ScalarType.string)),
      Chain.one(IntoFieldRaw("field", ScalarType.string))
    )

    // raw object
    val objectVar = VarRaw(
      "object",
      StructType("objectType", NonEmptyMap.one("field", ScalarType.string)),
      Chain.empty
    )

    // export object
    val getSrvTag = CallServiceTag(
      LiteralRaw.quote("getSrv"),
      "getObj",
      Call(Nil, Call.Export(objectVar.name, objectVar.`type`) :: Nil)
    )

    // function where we assign object lambda to value and call service
    val inner =
      FuncArrow(
        "inner",
        SeqTag.wrap(
          AssignmentTag(
            objectVarLambda,
            "fieldValue"
          ).leaf,
          CallServiceTag(
            LiteralRaw.quote("callbackSrv"),
            "response",
            Call(VarRaw("fieldValue", ScalarType.string) :: Nil, Nil)
          ).leaf
        ),
        ArrowType(
          ProductType.labelled((objectVar.name, objectVar.`type`) :: Nil),
          ProductType(Nil)
        ),
        Nil,
        Map.empty,
        Map.empty
      )

    // wrapper that export object and call inner function
    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            getSrvTag.leaf,
            CallArrowTag(inner.funcName, Call(objectVar :: Nil, Nil)).leaf
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        CallServiceModel(
          LiteralModel("\"getSrv\"", LiteralType.string),
          "getObj",
          CallModel(Nil, CallModel.Export(objectVar.name, objectVar.`type`) :: Nil)
        ).leaf,
        SeqModel.wrap(
          // TODO: redundant empty Seq, delete it somehow
          SeqModel.leaf,
          CallServiceModel(
            LiteralModel("\"callbackSrv\"", LiteralType.string),
            "response",
            CallModel(ValueModel.fromRaw(objectVarLambda) :: Nil, Nil)
          ).leaf
        )
      )
    ) should be(true)

  }

}
