package aqua.model.inline

import aqua.model.*
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.{ApplyPropertyRaw, FunctorRaw, IntoFieldRaw, IntoIndexRaw, LiteralRaw, VarRaw}
import aqua.types.*
import cats.syntax.show.*
import cats.syntax.option.*
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.raw.value.{CallArrowRaw, ValueRaw}
import aqua.raw.arrow.{ArrowRaw, FuncRaw}

class ArrowInlinerSpec extends AnyFlatSpec with Matchers {

  "arrow inliner" should "convert simple arrow" in {

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          CallArrowRawTag.service(LiteralRaw.quote("dumb_srv_id"), "dumb", Call(Nil, Nil)).leaf,
          ArrowType(ProductType(Nil), ProductType(Nil)),
          Nil,
          Map.empty,
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      CallServiceModel(
        LiteralModel.quote("dumb_srv_id"),
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
    val canonName = streamVar.name + "_canon"
    val canonModel = VarModel(canonName, CanonStreamType(ScalarType.string))
    val cbType = ArrowType(ProductType(ArrayType(ScalarType.string) :: Nil), ProductType(Nil))
    val cbVal = VarModel("cb-pass", cbType)

    val cbArg = VarRaw("cbVar", ArrayType(ScalarType.string))

    val cbArrow = FuncArrow(
      "cb",
      CallArrowRawTag
        .service(
          LiteralRaw.quote("test-service"),
          "some-call",
          Call(cbArg :: Nil, Nil)
        )
        .leaf,
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
      Map.empty,
      None
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "stream-callback",
          RestrictionTag(streamVar.name, streamType).wrap(
            SeqTag.wrap(
              DeclareStreamTag(streamVar).leaf,
              CallArrowRawTag.func("cb", Call(streamVar :: Nil, Nil)).leaf
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
          Map.empty,
          None
        ),
        CallModel(cbVal :: Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      RestrictionModel(streamVar.name, streamType).wrap(
        MetaModel
          .CallArrowModel("cb")
          .wrap(
            SeqModel.wrap(
              CanonicalizeModel(
                streamModel,
                CallModel.Export(canonModel.name, canonModel.`type`)
              ).leaf,
              CallServiceModel(
                LiteralModel.quote("test-service"),
                "some-call",
                CallModel(canonModel :: Nil, Nil)
              ).leaf
            )
          )
      )
    ) should be(true)

  }

  /*
    func stream-callback(cb: string -> ()):
      records: *string
      cb(records!)
   */
  ignore /*"arrow inliner"*/ should "pass stream with gate to callback properly" in {
    val streamType = StreamType(ScalarType.string)
    val streamVar = VarRaw("records", streamType)
    val streamVarLambda =
      ApplyPropertyRaw(
        VarRaw("records", streamType),
        IntoIndexRaw(LiteralRaw.number(0), ScalarType.string)
      )
    val streamModel = VarModel(
      "records",
      StreamType(ScalarType.string),
      Chain.one(IntoIndexModel("0", ScalarType.string))
    )
    val cbType = ArrowType(ProductType(ScalarType.string :: Nil), ProductType(Nil))
    val cbVal = VarModel("cb-pass", cbType)

    val cbArg =
      VarRaw(
        "cbVar",
        ScalarType.string
      )

    val cbArrow = FuncArrow(
      "cb",
      CallArrowRawTag
        .service(
          LiteralRaw.quote("test-service"),
          "some-call",
          Call(cbArg :: Nil, Nil)
        )
        .leaf,
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
      Map.empty,
      None
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "stream-callback",
          RestrictionTag(streamVar.name, streamType).wrap(
            SeqTag.wrap(
              DeclareStreamTag(streamVar).leaf,
              CallArrowRawTag.func("cb", Call(streamVarLambda :: Nil, Nil)).leaf
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
          Map.empty,
          None
        ),
        CallModel(cbVal :: Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      RestrictionModel(streamVar.name, streamType).wrap(
        CallServiceModel(
          LiteralModel.quote("test-service"),
          "some-call",
          CallModel(streamModel :: Nil, Nil)
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
    val canonType = CanonStreamType(ArrayType(ScalarType.string))
    val recordsVar = VarRaw("records", streamType)
    val recordsModel = VarModel(recordsVar.name, recordsVar.baseType)
    val canonModel = VarModel(recordsVar.name + "_canon", canonType)
    val innerRecordsVar = VarRaw("inner-records", StreamType(ArrayType(ScalarType.string)))
    val innerName = "inner"

    val inner = FuncArrow(
      innerName,
      CallArrowRawTag
        .service(
          LiteralRaw.quote("test-service"),
          "get_records",
          Call(Nil, Call.Export(innerRecordsVar.name, streamType) :: Nil)
        )
        .leaf,
      ArrowType(
        ProductType.labelled((innerRecordsVar.name -> streamType) :: Nil),
        ProductType(Nil)
      ),
      Nil,
      Map.empty,
      Map.empty,
      None
    )

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "outer",
          SeqTag.wrap(
            DeclareStreamTag(recordsVar).leaf,
            CallArrowRawTag.func(innerName, Call(recordsVar :: Nil, Nil)).leaf,
            CallArrowRawTag
              .service(
                LiteralRaw.quote("callbackSrv"),
                "response",
                Call(recordsVar :: Nil, Nil)
              )
              .leaf
          ),
          ArrowType(ProductType(Nil), ProductType(returnType :: Nil)),
          Nil,
          Map(innerName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        MetaModel
          .CallArrowModel(innerName)
          .wrap(
            CallServiceModel(
              LiteralModel.quote("test-service"),
              "get_records",
              CallModel(Nil, CallModel.Export(recordsModel.name, recordsModel.`type`) :: Nil)
            ).leaf
          ),
        SeqModel.wrap(
          CanonicalizeModel(recordsModel, CallModel.Export(canonModel.name, canonType)).leaf,
          CallServiceModel(
            LiteralModel.quote("callbackSrv"),
            "response",
            CallModel(canonModel :: Nil, Nil)
          ).leaf
        )
      )
    ) should be(true)

  }

  /**
   * service Test("test-service"):
   *   get_number() -> u16
   *
   * func inner() -> u16:
   *   res <- Test.get_number()
   *   <- res
   *
   * func outer() -> u16
   *   res1 <- inner() -- Meta should be left here
   *   res2 <- Test.get_number()
   *   res3 <- inner() -- Meta should be left here
   *   retval = res1 + res2 + res3
   *   <- retval
   */
  "arrow inliner" should "leave meta after function inlining" in {
    val innerName = "inner"
    val innerRes = VarRaw("res", ScalarType.u16)

    val serviceName = "test-service"
    val serviceMethod = "get_number"

    val serviceCall = (res: VarRaw) =>
      CallArrowRawTag
        .service(
          LiteralRaw.quote(serviceName),
          serviceMethod,
          Call(Nil, Call.Export(res.name, ScalarType.u16) :: Nil)
        )
        .leaf

    val innerBody = SeqTag.wrap(
      serviceCall(innerRes),
      ReturnTag(
        NonEmptyList.one(
          innerRes
        )
      ).leaf
    )

    val inner = FuncArrow(
      funcName = innerName,
      body = innerBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(innerRes),
      capturedArrows = Map.empty,
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val outterRes1 = VarRaw("res1", ScalarType.u16)
    val outterRes2 = VarRaw("res2", ScalarType.u16)
    val outterRes3 = VarRaw("res3", ScalarType.u16)
    val outterRetVal = VarRaw("retval", ScalarType.u16)

    val innerCall = (res: VarRaw) =>
      CallArrowRawTag
        .func(
          innerName,
          Call(Nil, Call.Export(res.name, ScalarType.u16) :: Nil)
        )
        .leaf

    val outerBody = SeqTag.wrap(
      innerCall(outterRes1),
      serviceCall(outterRes2),
      innerCall(outterRes3),
      AssignmentTag(
        RawBuilder.add(
          outterRes1,
          RawBuilder.add(
            outterRes2,
            outterRes3
          )
        ),
        outterRetVal.name
      ).leaf,
      ReturnTag(
        NonEmptyList
          .one(
            outterRetVal
          )
      ).leaf
    )

    val outer = FuncArrow(
      funcName = "outer",
      body = outerBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(outterRetVal),
      capturedArrows = Map(innerName -> inner),
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val model = ArrowInliner
      .callArrow[InliningState](
        outer,
        CallModel(Nil, Nil)
      )
      .runA(InliningState())
      .value

    val serviceCallModel = (res: VarModel) =>
      CallServiceModel(
        LiteralModel.quote(serviceName),
        serviceMethod,
        CallModel(Nil, CallModel.Export(res.name, res.`type`) :: Nil)
      ).leaf

    /* WARNING: This naming is unstable */
    val res1 = VarModel("res", ScalarType.u16)
    val res2 = VarModel("res2", ScalarType.u16)
    val res3 = VarModel("res-0", ScalarType.u16)
    val tempAdd = VarModel("add-0", ScalarType.u16)

    val expected = SeqModel.wrap(
      MetaModel
        .CallArrowModel(innerName)
        .wrap(
          serviceCallModel(res1)
        ),
      serviceCallModel(res2),
      MetaModel
        .CallArrowModel(innerName)
        .wrap(
          serviceCallModel(res3)
        ),
      SeqModel.wrap(
        ModelBuilder.add(res2, res3)(tempAdd).leaf,
        ModelBuilder.add(res1, tempAdd)(VarModel("add", ScalarType.u16)).leaf
      )
    )

    model.equalsOrShowDiff(expected) shouldEqual true
  }

  /**
   * func inner() -> u16:
   *   res = 42
   *   <- res
   *
   * func outer() -> u16:
   *   retval = inner() + inner() + 37
   *   <- retval
   */
  "arrow inliner" should "omit meta if arrow was completely erased" in {
    val innerName = "inner"
    val innerRes = VarRaw("res", ScalarType.u16)
    val innerRet = "42"

    val innerBody = SeqTag.wrap(
      AssignmentTag(
        LiteralRaw(innerRet, ScalarType.u16),
        innerRes.name
      ).leaf,
      ReturnTag(
        NonEmptyList.one(
          innerRes
        )
      ).leaf
    )

    val inner = FuncArrow(
      funcName = innerName,
      body = innerBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(innerRes),
      capturedArrows = Map.empty,
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val innerCall = CallArrowRaw(
      ability = None,
      name = innerName,
      arguments = Nil,
      baseType = ArrowType(
        domain = NilType,
        codomain = ProductType(List(ScalarType.u16))
      ),
      serviceId = None
    )

    val outerAdd = "37"
    val outterRetVal = VarRaw("retval", ScalarType.u16)

    val outerBody = SeqTag.wrap(
      AssignmentTag(
        RawBuilder.add(
          innerCall,
          RawBuilder.add(
            innerCall,
            LiteralRaw(outerAdd, ScalarType.u16)
          )
        ),
        outterRetVal.name
      ).leaf,
      ReturnTag(
        NonEmptyList
          .one(
            outterRetVal
          )
      ).leaf
    )

    val outer = FuncArrow(
      funcName = "outer",
      body = outerBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(outterRetVal),
      capturedArrows = Map(innerName -> inner),
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val model = ArrowInliner
      .callArrow[InliningState](
        outer,
        CallModel(Nil, Nil)
      )
      .runA(InliningState())
      .value

    /* WARNING: This naming is unstable */
    val tempAdd0 = VarModel("add-0", ScalarType.u16)
    val tempAdd = VarModel("add", ScalarType.u16)

    val expected = SeqModel.wrap(
      ModelBuilder
        .add(
          LiteralModel(innerRet, ScalarType.u16),
          LiteralModel(outerAdd, ScalarType.u16)
        )(tempAdd0)
        .leaf,
      ModelBuilder
        .add(
          LiteralModel(innerRet, ScalarType.u16),
          tempAdd0
        )(tempAdd)
        .leaf
    )

    model.equalsOrShowDiff(expected) shouldEqual true
  }

  /**
   * closureName = (x: u16) -> u16:
   *   retval = x + add
   *   <- retval
   *
   * @return (closure func, closure type, closure type labelled)
   */
  def addClosure(
    closureName: String,
    add: ValueRaw
  ): (FuncRaw, ArrowType, ArrowType) = {
    val closureArg = VarRaw(
      "x",
      ScalarType.u16
    )
    val closureRes = VarRaw(
      "retval",
      ScalarType.u16
    )
    val closureType = ArrowType(
      domain = ProductType(List(closureArg.`type`)),
      codomain = ProductType(List(ScalarType.u16))
    )
    val closureTypeLabelled = closureType.copy(
      domain = ProductType.labelled(List(closureArg.name -> closureArg.`type`))
    )

    val closureBody = SeqTag.wrap(
      AssignmentTag(
        RawBuilder.add(
          closureArg,
          add
        ),
        closureRes.name
      ).leaf,
      ReturnTag(
        NonEmptyList.one(closureRes)
      ).leaf
    )

    val closureFunc = FuncRaw(
      name = closureName,
      arrow = ArrowRaw(
        `type` = closureTypeLabelled,
        ret = List(closureRes),
        body = closureBody
      )
    )

    (closureFunc, closureType, closureTypeLabelled)
  }

  /**
   * func innerName(arg: u16) -> u16 -> u16:
   *   closureName = (x: u16) -> u16:
   *     retval = x + arg
   *     <- retval
   *   <- closureName
   *
   * func outer() -> u16:
   *   outterClosureName <- inner(42)
   *   <body(outterClosureName.type)>
   *   <- outterResultName
   */
  def closureReturnModel(
    innerName: String,
    closureName: String,
    outterClosureName: String,
    outterResultName: String,
    body: (ArrowType) => List[RawTag.Tree]
  ) = {
    val innerArg = VarRaw(
      "arg",
      ScalarType.u16
    )

    val (closureFunc, closureType, closureTypeLabelled) =
      addClosure(closureName, innerArg)

    val innerRes = VarRaw(
      closureName,
      closureTypeLabelled
    )
    val innerType = ArrowType(
      domain = ProductType.labelled(List(innerArg.name -> innerArg.`type`)),
      codomain = ProductType(List(closureType))
    )

    val innerBody = SeqTag.wrap(
      ClosureTag(
        func = closureFunc,
        detach = false
      ).leaf,
      ReturnTag(
        NonEmptyList.one(innerRes)
      ).leaf
    )

    val inner = FuncArrow(
      funcName = innerName,
      body = innerBody,
      arrowType = innerType,
      ret = List(innerRes),
      capturedArrows = Map.empty,
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val outterClosure = VarRaw(
      "c",
      closureType
    )
    val outterRes = VarRaw(
      "retval",
      ScalarType.u16
    )

    val innerCall =
      CallArrowRawTag(
        List(Call.Export(outterClosure.name, outterClosure.`type`)),
        CallArrowRaw(
          ability = None,
          name = innerName,
          arguments = List(LiteralRaw("42", LiteralType.number)),
          baseType = innerType,
          serviceId = None
        )
      ).leaf

    val outerBody = SeqTag.wrap(
      innerCall +: body(closureType) :+ ReturnTag(
        NonEmptyList
          .one(
            outterRes
          )
      ).leaf: _*
    )

    val outer = FuncArrow(
      funcName = "outer",
      body = outerBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(outterRes),
      capturedArrows = Map(innerName -> inner),
      capturedValues = Map.empty,
      capturedTopology = None
    )

    ArrowInliner
      .callArrow[InliningState](
        outer,
        CallModel(Nil, Nil)
      )
      .runA(InliningState())
      .value
  }

  /**
   * func inner(arg: u16) -> u16 -> u16:
   *   closure = (x: u16) -> u16:
   *     retval = x + arg
   *     <- retval
   *   <- closure
   *
   * func outer() -> u16:
   *   c <- inner(42)
   *   retval = 37 + c(1) + c(2)
   *   <- retval
   */
  "arrow inliner" should "leave meta after returned closure inlining" in {
    val innerName = "inner"
    val closureName = "closure"
    val outterClosureName = "c"
    val outterResultName = "retval"

    val closureCall = (closureType: ArrowType, i: String) =>
      CallArrowRaw(
        ability = None,
        name = outterClosureName,
        arguments = List(LiteralRaw(i, LiteralType.number)),
        baseType = closureType,
        serviceId = None
      )

    val body = (closureType: ArrowType) =>
      List(
        AssignmentTag(
          RawBuilder.add(
            RawBuilder.add(
              LiteralRaw("37", LiteralType.number),
              closureCall(closureType, "1")
            ),
            closureCall(closureType, "2")
          ),
          outterResultName
        ).leaf
      )

    val model = closureReturnModel(
      innerName = innerName,
      closureName = closureName,
      outterClosureName = outterClosureName,
      outterResultName = outterResultName,
      body = body
    )

    val closureCallModel = (x: String, o: VarModel) =>
      MetaModel
        .CallArrowModel(closureName)
        .wrap(
          ApplyTopologyModel(closureName)
            .wrap(
              ModelBuilder
                .add(
                  LiteralModel(x, LiteralType.number),
                  LiteralModel("42", LiteralType.number)
                )(o)
                .leaf
            )
        )

    /* WARNING: This naming is unstable */
    val tempAdd0 = VarModel("add-0", ScalarType.u16)
    val tempAdd1 = VarModel("add-1", ScalarType.u16)
    val tempAdd2 = VarModel("add-2", ScalarType.u16)
    val tempAdd = VarModel("add", ScalarType.u16)

    val expected = SeqModel.wrap(
      MetaModel
        .CallArrowModel(innerName)
        .wrap(
          CaptureTopologyModel(closureName).leaf
        ),
      SeqModel.wrap(
        ParModel.wrap(
          SeqModel.wrap(
            closureCallModel("1", tempAdd1),
            ModelBuilder
              .add(
                LiteralModel("37", LiteralType.number),
                tempAdd1
              )(tempAdd0)
              .leaf
          ),
          closureCallModel("2", tempAdd2)
        ),
        ModelBuilder
          .add(
            tempAdd0,
            tempAdd2
          )(tempAdd)
          .leaf
      )
    )

    model.equalsOrShowDiff(expected) shouldEqual true
  }

  /**
   * func inner() -> () -> u16:
   *   closure = func () -> u16:
   *     <- 42
   *   <- closure
   *
   * func outer() -> u16:
   *   c <- inner()
   *   retval = 37 + c() + c()
   *   <- retval
   */
  "arrow inliner" should "omit meta if returned closure was completely erased" in {
    val innerName = "inner"
    val closureName = "closure"

    val closureRes = LiteralRaw(
      "42",
      LiteralType.number
    )
    val closureType = ArrowType(
      domain = NilType,
      codomain = ProductType(List(ScalarType.u16))
    )

    val innerRes = VarRaw(
      closureName,
      closureType
    )
    val innerType = ArrowType(
      domain = NilType,
      codomain = ProductType(List(closureType))
    )

    val closureBody = SeqTag.wrap(
      ReturnTag(
        NonEmptyList.one(closureRes)
      ).leaf
    )

    val closureFunc = FuncRaw(
      name = closureName,
      arrow = ArrowRaw(
        `type` = closureType,
        ret = List(closureRes),
        body = closureBody
      )
    )

    val innerBody = SeqTag.wrap(
      ClosureTag(
        func = closureFunc,
        detach = true
      ).leaf,
      ReturnTag(
        NonEmptyList.one(innerRes)
      ).leaf
    )

    val inner = FuncArrow(
      funcName = innerName,
      body = innerBody,
      arrowType = innerType,
      ret = List(innerRes),
      capturedArrows = Map.empty,
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val outterClosure = VarRaw(
      "c",
      closureType
    )
    val outterRes = VarRaw(
      "retval",
      ScalarType.u16
    )

    val innerCall =
      CallArrowRawTag(
        List(Call.Export(outterClosure.name, outterClosure.`type`)),
        CallArrowRaw(
          ability = None,
          name = innerName,
          arguments = Nil,
          baseType = innerType,
          serviceId = None
        )
      ).leaf

    val closureCall =
      CallArrowRaw(
        ability = None,
        name = outterClosure.name,
        arguments = Nil,
        baseType = closureType,
        serviceId = None
      )

    val outerBody = SeqTag.wrap(
      innerCall,
      AssignmentTag(
        RawBuilder.add(
          RawBuilder.add(
            LiteralRaw("37", LiteralType.number),
            closureCall
          ),
          closureCall
        ),
        outterRes.name
      ).leaf,
      ReturnTag(
        NonEmptyList
          .one(
            outterRes
          )
      ).leaf
    )

    val outer = FuncArrow(
      funcName = "outer",
      body = outerBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(outterRes),
      capturedArrows = Map(innerName -> inner),
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val model = ArrowInliner
      .callArrow[InliningState](
        outer,
        CallModel(Nil, Nil)
      )
      .runA(InliningState())
      .value

    /* WARNING: This naming is unstable */
    val tempAdd0 = VarModel("add-0", ScalarType.u16)
    val tempAdd = VarModel("add", ScalarType.u16)

    val number = (v: String) =>
      LiteralModel(
        v,
        LiteralType.number
      )

    val expected = SeqModel.wrap(
      ModelBuilder
        .add(
          number("37"),
          number("42")
        )(tempAdd0)
        .leaf,
      ModelBuilder
        .add(
          tempAdd0,
          number("42")
        )(tempAdd)
        .leaf
    )

    model.equalsOrShowDiff(expected) shouldEqual true
  }

  /**
   * func inner(arg: u16) -> u16 -> u16:
   *   closure = (x: u16) -> u16:
   *     retval = x + arg
   *     <- retval
   *   <- closure
   *
   * func outer() -> u16:
   *   c <- inner(42)
   *   b = c
   *   a = b
   *   retval = 37 + a(1) + b(2) + c{3}
   *   <- retval
   */
  "arrow inliner" should "correctly inline renamed closure [bug LNG-193]" in {
    val innerName = "inner"
    val closureName = "closure"
    val outterClosureName = "c"
    val outterResultName = "retval"
    val firstRename = "b"
    val secondRename = "a"

    val closureCall = (name: String, closureType: ArrowType, i: String) =>
      CallArrowRaw(
        ability = None,
        name = name,
        arguments = List(LiteralRaw(i, LiteralType.number)),
        baseType = closureType,
        serviceId = None
      )

    val body = (closureType: ArrowType) =>
      List(
        AssignmentTag(
          VarRaw(outterClosureName, closureType),
          firstRename
        ).leaf,
        AssignmentTag(
          VarRaw(firstRename, closureType),
          secondRename
        ).leaf,
        AssignmentTag(
          RawBuilder
            .add(
              RawBuilder.add(
                RawBuilder.add(
                  LiteralRaw("37", LiteralType.number),
                  closureCall(secondRename, closureType, "1")
                ),
                closureCall(firstRename, closureType, "2")
              ),
              closureCall(outterClosureName, closureType, "3")
            ),
          outterResultName
        ).leaf
      )

    val model = closureReturnModel(
      innerName = innerName,
      closureName = closureName,
      outterClosureName = outterClosureName,
      outterResultName = outterResultName,
      body = body
    )

    val closureCallModel = (x: String, o: VarModel) =>
      MetaModel
        .CallArrowModel(closureName)
        .wrap(
          ApplyTopologyModel(closureName)
            .wrap(
              ModelBuilder
                .add(
                  LiteralModel(x, LiteralType.number),
                  LiteralModel("42", LiteralType.number)
                )(o)
                .leaf
            )
        )

    /* WARNING: This naming is unstable */
    val tempAdd0 = VarModel("add-0", ScalarType.u16)
    val tempAdd1 = VarModel("add-1", ScalarType.u16)
    val tempAdd2 = VarModel("add-2", ScalarType.u16)
    val tempAdd3 = VarModel("add-3", ScalarType.u16)
    val tempAdd4 = VarModel("add-4", ScalarType.u16)
    val tempAdd = VarModel("add", ScalarType.u16)

    val expected = SeqModel.wrap(
      MetaModel
        .CallArrowModel(innerName)
        .wrap(
          CaptureTopologyModel(closureName).leaf
        ),
      SeqModel.wrap(
        ParModel.wrap(
          SeqModel.wrap(
            ParModel.wrap(
              SeqModel.wrap(
                closureCallModel("1", tempAdd2),
                ModelBuilder
                  .add(
                    LiteralModel("37", LiteralType.number),
                    tempAdd2
                  )(tempAdd1)
                  .leaf
              ),
              closureCallModel("2", tempAdd3)
            ),
            ModelBuilder
              .add(
                tempAdd1,
                tempAdd3
              )(tempAdd0)
              .leaf
          ),
          closureCallModel("3", tempAdd4)
        ),
        ModelBuilder
          .add(
            tempAdd0,
            tempAdd4
          )(tempAdd)
          .leaf
      )
    )

    model.equalsOrShowDiff(expected) shouldEqual true
  }

  /**
   * func accept_closure(closure: u16 -> u16) -> u16:
   *   resA <- closure(42)
   *   <- resA
   *
   * func test() -> u16:
   *   closure = (x: u16) -> u16:
   *     resC = x + 37
   *     <- resC
   *   resT <- accept_closure(closure)
   *   <- resT
   */
  "arrow inliner" should "correctly handle closure as argument [bug LNG-92]" in {
    val acceptName = "accept_closure"
    val closureName = "closure"
    val testName = "test"
    val acceptRes = VarRaw("resA", ScalarType.u16)
    val testRes = VarRaw("resT", ScalarType.u16)

    val (closureFunc, closureType, closureTypeLabelled) =
      addClosure(closureName, LiteralRaw("37", LiteralType.number))

    val acceptType = ArrowType(
      domain = ProductType.labelled(List(closureName -> closureType)),
      codomain = ProductType(ScalarType.u16 :: Nil)
    )

    val acceptBody = SeqTag.wrap(
      CallArrowRawTag(
        List(Call.Export(acceptRes.name, acceptRes.baseType)),
        CallArrowRaw(
          ability = None,
          name = closureName,
          arguments = List(LiteralRaw("42", LiteralType.number)),
          baseType = closureType,
          serviceId = None
        )
      ).leaf,
      ReturnTag(
        NonEmptyList.one(acceptRes)
      ).leaf
    )

    val acceptFunc = FuncArrow(
      funcName = acceptName,
      body = acceptBody,
      arrowType = ArrowType(
        ProductType.labelled(List(closureName -> closureType)),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(acceptRes),
      capturedArrows = Map.empty,
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val testBody = SeqTag.wrap(
      ClosureTag(
        func = closureFunc,
        detach = false
      ).leaf,
      CallArrowRawTag(
        List(Call.Export(testRes.name, testRes.baseType)),
        CallArrowRaw(
          ability = None,
          name = acceptName,
          arguments = List(VarRaw(closureName, closureTypeLabelled)),
          baseType = acceptFunc.arrowType,
          serviceId = None
        )
      ).leaf,
      ReturnTag(
        NonEmptyList.one(testRes)
      ).leaf
    )

    val testFunc = FuncArrow(
      funcName = testName,
      body = testBody,
      arrowType = ArrowType(
        ProductType(Nil),
        ProductType(List(ScalarType.u16))
      ),
      ret = List(testRes),
      capturedArrows = Map(acceptName -> acceptFunc),
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val model = ArrowInliner
      .callArrow[InliningState](
        testFunc,
        CallModel(Nil, Nil)
      )
      .runA(InliningState())
      .value

    /* WARNING: This naming is unstable */
    val tempAdd = VarModel("add", ScalarType.u16)

    val expected = SeqModel.wrap(
      CaptureTopologyModel(closureName).leaf,
      MetaModel
        .CallArrowModel(acceptName)
        .wrap(
          MetaModel
            .CallArrowModel(closureName)
            .wrap(
              ApplyTopologyModel(closureName).wrap(
                ModelBuilder
                  .add(
                    LiteralModel("42", LiteralType.number),
                    LiteralModel("37", LiteralType.number)
                  )(tempAdd)
                  .leaf
              )
            )
        )
    )

    model.equalsOrShowDiff(expected) shouldEqual true
  }

  /**
   * service Test("test-service"):
   *   method(method: string) -> string
   *
   * func test(method: string) -> string:
   *   res <- Test.method(method)
   *   <- res
   *
   * func main():
   *   method = "method"
   *   test(method)
   */
  "arrow inliner" should "not rename service call [bug LNG-199]" in {
    val testName = "test"
    val argMethodName = "method"
    val serviceName = "Test"
    val serviceId = LiteralRaw("test-service", LiteralType.string)
    val res = VarRaw("res", ScalarType.string)

    val testType = ArrowType(
      domain = ProductType.labelled(List(argMethodName -> ScalarType.string)),
      codomain = ProductType(ScalarType.string :: Nil)
    )

    val testBody = SeqTag.wrap(
      CallArrowRawTag
        .service(
          serviceId = serviceId,
          fnName = argMethodName,
          call = Call(
            args = VarRaw(argMethodName, ScalarType.string) :: Nil,
            exportTo = Call.Export(res.name, res.`type`) :: Nil
          ),
          name = serviceName,
          arrowType = ArrowType(
            domain = ProductType.labelled(List(argMethodName -> ScalarType.string)),
            codomain = ProductType(ScalarType.string :: Nil)
          )
        )
        .leaf,
      ReturnTag(
        NonEmptyList.one(res)
      ).leaf
    )

    val testFunc = FuncArrow(
      funcName = testName,
      body = testBody,
      arrowType = testType,
      ret = Nil,
      capturedArrows = Map.empty,
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val mainType = ArrowType(
      domain = ProductType(Nil),
      codomain = ProductType(Nil)
    )

    val mainBody = SeqTag.wrap(
      AssignmentTag(
        LiteralRaw.quote(argMethodName),
        argMethodName
      ).leaf,
      CallArrowRawTag
        .func(
          fnName = testName,
          call = Call(args = VarRaw(argMethodName, LiteralType.string) :: Nil, Nil)
        )
        .leaf
    )

    val mainFunc = FuncArrow(
      funcName = "main",
      body = mainBody,
      arrowType = ArrowType(ProductType(Nil), ProductType(Nil)),
      ret = Nil,
      capturedArrows = Map(testName -> testFunc),
      capturedValues = Map.empty,
      capturedTopology = None
    )

    val model = ArrowInliner
      .callArrow[InliningState](
        mainFunc,
        CallModel(Nil, Nil)
      )
      .runA(InliningState())
      .value

    val expected = MetaModel
      .CallArrowModel(testName)
      .wrap(
        CallServiceModel(
          serviceId = ValueModel.fromRaw(serviceId),
          funcName = argMethodName,
          call = CallModel(
            args = LiteralModel.quote(argMethodName) :: Nil,
            exportTo = CallModel.Export(res.name, res.`type`) :: Nil
          )
        ).leaf
      )

    model.equalsOrShowDiff(expected) shouldEqual true
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
    val innerName = "inner"

    // lambda that will be assigned to another variable
    val objectVarLambda =
      VarRaw("object", StructType("objectType", NonEmptyMap.one("field", ScalarType.string)))
        .withProperty(
          IntoFieldRaw("field", ScalarType.string)
        )

    val flattenObject = VarRaw("object_flat", ScalarType.string)

    // raw object
    val objectVar = VarRaw(
      "object",
      StructType("objectType", NonEmptyMap.one("field", ScalarType.string))
    )

    // export object
    val getSrvTag = CallArrowRawTag.service(
      LiteralRaw.quote("getSrv"),
      "getObj",
      Call(Nil, Call.Export(objectVar.name, objectVar.`type`) :: Nil)
    )

    // function where we assign object lambda to value and call service
    val inner =
      FuncArrow(
        innerName,
        SeqTag.wrap(
          AssignmentTag(
            objectVarLambda,
            "fieldValue"
          ).leaf,
          CallArrowRawTag
            .service(
              LiteralRaw.quote("callbackSrv"),
              "response",
              Call(VarRaw("fieldValue", ScalarType.string) :: Nil, Nil)
            )
            .leaf
        ),
        ArrowType(
          ProductType.labelled((objectVar.name, objectVar.`type`) :: Nil),
          ProductType(Nil)
        ),
        Nil,
        Map.empty,
        Map.empty,
        None
      )

    // wrapper that export object and call inner function
    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            getSrvTag.leaf,
            CallArrowRawTag.func(inner.funcName, Call(objectVar :: Nil, Nil)).leaf
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        CallServiceModel(
          LiteralModel.quote("getSrv"),
          "getObj",
          CallModel(Nil, CallModel.Export(objectVar.name, objectVar.`type`) :: Nil)
        ).leaf,
        MetaModel
          .CallArrowModel(innerName)
          .wrap(
            SeqModel.wrap(
              FlattenModel(ValueModel.fromRaw(objectVarLambda), flattenObject.name).leaf,
              CallServiceModel(
                LiteralModel.quote("callbackSrv"),
                "response",
                CallModel(ValueModel.fromRaw(flattenObject) :: Nil, Nil)
              ).leaf
            )
          )
      )
    ) should be(true)

  }

  /*
  func joinIdxLocal(idx: i16, nodes: []string):
    join nodes[idx]
   */
  "arrow inliner" should "not rename value in index array lambda" in {
    val innerName = "inner"

    // lambda that will be assigned to another variable
    val argArray = VarRaw(
      "nodes",
      ArrayType(ScalarType.string)
    )

    val idxVar = VarRaw("idx", ScalarType.u32)

    val arrIdx = VarRaw("nodes", ArrayType(ScalarType.string)).withProperty(
      IntoIndexRaw(idxVar, ScalarType.string)
    )

    val getArrTag = CallArrowRawTag
      .service(
        LiteralRaw.quote("getSrv"),
        "getArr",
        Call(Nil, Call.Export(argArray.name, argArray.`type`) :: Nil)
      )
      .leaf

    val getIdxTag = CallArrowRawTag
      .service(
        LiteralRaw.quote("getSrv"),
        "getIdx",
        Call(Nil, Call.Export(idxVar.name, idxVar.`type`) :: Nil)
      )
      .leaf

    // function where we assign object lambda to value and call service
    val inner =
      FuncArrow(
        innerName,
        JoinTag(NonEmptyList.one(arrIdx)).leaf,
        ArrowType(
          ProductType.labelled(
            (idxVar.name, idxVar.`type`) :: (argArray.name, argArray.`type`) :: Nil
          ),
          ProductType(Nil)
        ),
        Nil,
        Map.empty,
        Map.empty,
        None
      )

    // wrapper that export object and call inner function
    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            getArrTag,
            getIdxTag,
            CallArrowRawTag.func(inner.funcName, Call(idxVar :: argArray :: Nil, Nil)).leaf
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      SeqModel.wrap(
        CallServiceModel(
          LiteralModel.quote("getSrv"),
          "getArr",
          CallModel(Nil, CallModel.Export(argArray.name, argArray.`type`) :: Nil)
        ).leaf,
        CallServiceModel(
          LiteralModel.quote("getSrv"),
          "getIdx",
          CallModel(Nil, CallModel.Export(idxVar.name, idxVar.`type`) :: Nil)
        ).leaf
      )
    ) should be(true)

  }

  "arrow inliner" should "rename value in arrow with same name as in for" in {
    val argVar = VarRaw("arg", ScalarType.u32)
    val iVar = VarRaw("i", ScalarType.string)
    val iVar0 = VarRaw("i-0", ScalarType.string)
    val innerVar = VarRaw("i", ScalarType.u32)
    val returnVar = VarRaw("ret", ScalarType.u32)

    val array = VarRaw(
      "nodes",
      ArrayType(ScalarType.string)
    )

    val inner =
      FuncArrow(
        "inner",
        ReturnTag(NonEmptyList.one(innerVar)).leaf,
        ArrowType(
          ProductType.labelled(
            (innerVar.name, innerVar.`type`) :: Nil
          ),
          ProductType(innerVar.`type` :: Nil)
        ),
        innerVar :: Nil,
        Map.empty,
        Map.empty,
        None
      )

    val serviceId = LiteralRaw.quote("test-service")
    val fnName = "some-call"

    val inFold = SeqTag.wrap(
      CallArrowRawTag
        .func(
          inner.funcName,
          Call(argVar :: Nil, Call.Export(returnVar.name, returnVar.`type`) :: Nil)
        )
        .leaf,
      CallArrowRawTag
        .service(
          serviceId,
          fnName,
          Call(returnVar :: Nil, Nil)
        )
        .leaf
    )

    val foldOp =
      ForTag(iVar.name, array, ForTag.Mode.Wait.some).wrap(inFold, NextTag(iVar.name).leaf)

    val model: OpModel.Tree = ArrowInliner
      .callArrow[InliningState](
        FuncArrow(
          "dumb_func",
          SeqTag.wrap(
            AssignmentTag(LiteralRaw("1", LiteralType.number), argVar.name).leaf,
            foldOp
          ),
          ArrowType(
            ProductType(Nil),
            ProductType(Nil)
          ),
          Nil,
          Map(inner.funcName -> inner),
          Map.empty,
          None
        ),
        CallModel(Nil, Nil)
      )
      .run(InliningState())
      .value
      ._2

    model.equalsOrShowDiff(
      ForModel(iVar0.name, ValueModel.fromRaw(array), ForModel.Mode.Never.some).wrap(
        CallServiceModel(
          LiteralModel.fromRaw(serviceId),
          fnName,
          CallModel(LiteralModel("1", LiteralType.number) :: Nil, Nil)
        ).leaf,
        NextModel(iVar0.name).leaf
      )
    ) should be(true)
  }

  /*
  service Get("get"):
      get() -> string

  func inner() -> string:
      results <- DTGetter.get_dt()
      <- results

  func outer() -> []string:
      results: *string
      results <- use_name1()
      <- results
   */
  "arrow inliner" should "generate result in right order" in {
    val innerName = "inner"
    val results = VarRaw("results", ScalarType.string)
    val resultsOut = VarRaw("results", StreamType(ScalarType.string))

    val inner = FuncArrow(
      innerName,
      SeqTag.wrap(
        CallArrowRawTag
          .service(
            LiteralRaw.quote("Get"),
            "get",
            Call(Nil, Call.Export(results.name, results.baseType) :: Nil)
          )
          .leaf
      ),
      ArrowType(
        ProductType(Nil),
        ProductType(ScalarType.string :: Nil)
      ),
      results :: Nil,
      Map.empty,
      Map.empty,
      None
    )

    val captured = Map.apply((innerName, inner))

    val outer = FuncArrow(
      "outer",
      SeqTag.wrap(
        DeclareStreamTag(resultsOut).leaf,
        CallArrowRawTag
          .func(innerName, Call(Nil, Call.Export(resultsOut.name, resultsOut.baseType) :: Nil))
          .leaf
      ),
      ArrowType(
        ProductType(Nil),
        ProductType(ArrayType(ScalarType.string) :: Nil)
      ),
      resultsOut :: Nil,
      captured,
      Map.empty,
      None
    )

    val (state, model: OpModel.Tree) = ArrowInliner
      .callArrow[InliningState](outer, CallModel(Nil, Nil))
      .run(InliningState())
      .value

    val resultModel = VarModel("results-0", ScalarType.string)
    model.equalsOrShowDiff(
      MetaModel
        .CallArrowModel(innerName)
        .wrap(
          SeqModel.wrap(
            CallServiceModel(
              LiteralModel.quote("Get"),
              "get",
              CallModel(Nil, CallModel.Export(resultModel.name, resultModel.`type`) :: Nil)
            ).leaf,
            PushToStreamModel(
              resultModel,
              CallModel.Export(resultsOut.name, resultsOut.baseType)
            ).leaf
          )
        )
    ) should be(true)
  }

}
