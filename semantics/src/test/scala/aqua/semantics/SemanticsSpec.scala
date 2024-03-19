package aqua.semantics

import aqua.parser.Ast
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.raw.ConstantRaw
import aqua.raw.RawContext
import aqua.raw.ops.*
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp, OnTag, ParTag, RawTag, SeqGroupTag, SeqTag}
import aqua.raw.value.*
import aqua.types.*

import cats.Eval
import cats.data.State
import cats.data.Validated
import cats.data.{Chain, EitherNec, NonEmptyChain, NonEmptyMap}
import cats.free.Cofree
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.~>
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticsSpec extends AnyFlatSpec with Matchers with Inside {

  val emptyCall = Call(Nil, Nil)

  implicit val fileLift: LiftParser[Span.S] = Span.spanLiftParser
  val parser = Parser.parse(Parser.spanParser)

  val semantics = new RawSemantics[Span.S]()

  private def addAqua(script: String) =
    if (script.startsWith("aqua")) script else "aqua Test\n" + script

  def insideResult(script: String)(
    test: PartialFunction[
      (
        Chain[SemanticWarning[Span.S]],
        EitherNec[SemanticError[Span.S], RawContext]
      ),
      Any
    ]
  ): Unit = inside(parser(addAqua(script))) { case Validated.Valid(ast) =>
    val init = RawContext.blank.copy(
      parts = Chain
        .fromSeq(ConstantRaw.defaultConstants())
        .map(const => RawContext.blank -> const)
    )
    inside(semantics.process(ast, init).value.run)(test)
  }

  def insideBody(script: String, func: Option[String] = None)(test: RawTag.Tree => Any): Unit =
    insideResult(script) { case (_, Right(ctx)) =>
      inside(
        func.fold(
          ctx.funcs.headOption.map { case (_, raw) => raw }
        )(ctx.funcs.get)
      ) { case Some(func) => test(func.arrow.body) }
    }

  def insideSemErrors(script: String)(test: NonEmptyChain[SemanticError[Span.S]] => Any): Unit =
    inside(parser(addAqua(script))) { case Validated.Valid(ast) =>
      val init = RawContext.blank
      inside(semantics.process(ast, init).value.value) { case Left(errors) =>
        test(errors)
      }
    }

  def matchSubtree(tree: RawTag.Tree)(
    matcher: PartialFunction[(RawTag, RawTag.Tree), Any]
  ): Unit = {
    def evalMatch(t: RawTag.Tree): Eval[Option[Unit]] =
      if (matcher.isDefinedAt((t.head, t)))
        Eval.now(
          Some(
            matcher((t.head, t))
          )
        )
      else
        t.tail.flatMap(
          _.collectFirstSomeM(evalMatch)
        )

    evalMatch(tree).value.getOrElse(fail(s"Did not match subtree"))
  }

  def matchChildren(tree: RawTag.Tree)(
    matchers: PartialFunction[(RawTag, RawTag.Tree), Any]*
  ): Unit = {
    val children = tree.tail.value
    children should have size matchers.length
    children.toList.zip(matchers).foreach { case (child, matcher) =>
      matcher.lift((child.head, child)).getOrElse(fail(s"Unexpected child $child"))
    }
  }

  val testServiceDef = """
                         |service Test("test"):
                         |  testCall()
                         |  testCallStr(s: string) -> string
                         |
  """.stripMargin

  def testServiceCallStr(str: String) = {
    val arrowType = ArrowType(
      ProductType.labelled(("s" -> ScalarType.string) :: Nil),
      ProductType(ScalarType.string :: Nil)
    )

    CallArrowRawTag(
      Nil,
      ApplyPropertyRaw(
        VarRaw(
          "Test",
          ServiceType(
            "Test",
            NonEmptyMap.of(
              "testCallStr" -> arrowType,
              "testCall" -> ArrowType(NilType, NilType)
            )
          )
        ),
        IntoArrowRaw("testCallStr", arrowType, LiteralRaw.quote(str) :: Nil)
      )
    ).leaf
  }

  def equ(left: ValueRaw, right: ValueRaw): ApplyBinaryOpRaw =
    ApplyBinaryOpRaw(ApplyBinaryOpRaw.Op.Eq, left, right, ScalarType.bool)

  def neq(left: ValueRaw, right: ValueRaw): ApplyBinaryOpRaw =
    ApplyBinaryOpRaw(ApplyBinaryOpRaw.Op.Neq, left, right, ScalarType.bool)

  def declareStreamPush(
    name: String,
    value: String
  ): RawTag.Tree = {
    val streamType = StreamType(ScalarType.string)
    val stream = VarRaw(name, streamType)

    SeqTag.wrap(
      DeclareStreamTag(stream).leaf,
      PushToStreamTag(
        LiteralRaw.quote(value),
        Call.Export(name, streamType)
      ).leaf
    )
  }

  // use it to fix https://github.com/fluencelabs/aqua/issues/90
  "semantics" should "create right model" in {
    val script =
      """service A("srv1"):
        |    fn1: -> string
        |
        |func parFunc():           
        |    on "other-peer":
        |        A.fn1()       
        |    par A.fn1()""".stripMargin

    insideBody(script) { body =>
      val arrowType = ArrowType(NilType, ConsType.cons(ScalarType.string, NilType))
      val serviceCall = CallArrowRawTag(
        Nil,
        ApplyPropertyRaw(
          VarRaw(
            "A",
            ServiceType(
              "A",
              NonEmptyMap.of(
                "fn1" -> arrowType
              )
            )
          ),
          IntoArrowRaw("fn1", arrowType, Nil)
        )
      ).leaf

      val expected =
        ParTag.wrap(
          OnTag(
            LiteralRaw("\"other-peer\"", LiteralType.string),
            Chain.empty
          ).wrap(
            serviceCall
          ),
          serviceCall
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle if with else" in {
    val script = testServiceDef +
      """
        |func test():           
        |    if 1 == 2:
        |       Test.testCallStr("if")
        |    else:
        |       Test.testCallStr("else")
        |""".stripMargin

    insideBody(script) { body =>
      val expected =
        IfTag(equ(LiteralRaw.number(1), LiteralRaw.number(2))).wrap(
          testServiceCallStr("if"),
          testServiceCallStr("else")
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle try with catch" in {
    val script = testServiceDef +
      """
        |func test():           
        |    try:
        |       Test.testCallStr("try")
        |    catch e:
        |       Test.testCallStr("catch")
        |""".stripMargin

    insideBody(script) { body =>
      val expected =
        TryTag.wrap(
          testServiceCallStr("try"),
          SeqTag.wrap(
            AssignmentTag(ValueRaw.error, "e").leaf,
            testServiceCallStr("catch")
          )
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle try with more than one catch" in {
    val script = testServiceDef +
      """
        |func test():
        |    try:
        |       Test.testCallStr("try")
        |    catch e:
        |       Test.testCallStr("catch1")
        |    catch e:
        |       Test.testCallStr("catch2")
        |""".stripMargin

    insideBody(script) { body =>
      val expected =
        TryTag.wrap(
          testServiceCallStr("try"),
          SeqTag.wrap(
            AssignmentTag(ValueRaw.error, "e").leaf,
            testServiceCallStr("catch1")
          ),
          SeqTag.wrap(
            AssignmentTag(ValueRaw.error, "e").leaf,
            testServiceCallStr("catch2")
          )
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle try with otherwise" in {
    val script = testServiceDef +
      """
        |func test():           
        |    try:
        |       Test.testCallStr("try")
        |    otherwise:
        |       Test.testCallStr("otherwise")
        |""".stripMargin

    insideBody(script) { body =>
      val expected =
        TryTag.wrap(
          testServiceCallStr("try"),
          testServiceCallStr("otherwise")
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle if without else" in {
    val script = testServiceDef +
      """
        |func test():           
        |    if 1 != 2:
        |       Test.testCallStr("if")
        |""".stripMargin

    insideBody(script) { body =>
      val expected =
        IfTag(neq(LiteralRaw.number(1), LiteralRaw.number(2))).wrap(
          testServiceCallStr("if")
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle try without catch" in {
    val script = testServiceDef +
      """
        |func test():           
        |    try:
        |       Test.testCallStr("try")
        |""".stripMargin

    insideBody(script) { body =>
      val expected =
        TryTag.wrap(
          testServiceCallStr("try")
        )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle par" in {
    val tests = List("two", "three", "four", "five")

    (1 to tests.length)
      .map(tests.take(_))
      .foreach(test =>
        val script = testServiceDef +
          """
            |func test():           
            |    Test.testCallStr("one")
            |""".stripMargin +
          test.map(n => s"    par Test.testCallStr(\"$n\")\n").mkString

        insideBody(script) { body =>
          val expected = ParTag.wrap(
            testServiceCallStr("one") +: test.map(n => testServiceCallStr(n))
          )

          body.equalsOrShowDiff(expected) should be(true)
        }
      )
  }

  it should "handle otherwise" in {
    val script = testServiceDef +
      """
        |func test():           
        |    Test.testCallStr("fail")
        |    otherwise:
        |       Test.testCallStr("otherwise")
        |""".stripMargin

    insideBody(script) { body =>
      val expected = TryTag.wrap(
        testServiceCallStr("fail"),
        testServiceCallStr("otherwise")
      )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle if with otherwise" in {
    val script = testServiceDef +
      """
        |func test():           
        |    if "a" != "b":
        |       Test.testCallStr("if")
        |    otherwise:
        |       Test.testCallStr("otherwise")
        |""".stripMargin

    insideBody(script) { body =>
      val expected = TryTag.wrap(
        IfTag(neq(LiteralRaw.quote("a"), LiteralRaw.quote("b"))).wrap(
          testServiceCallStr("if")
        ),
        testServiceCallStr("otherwise")
      )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "handle if and try with par" in {
    val tests = List("two", "three", "four", "five")
    val ifTry = List(
      """
        |    if "a" != "b":
        |       Test.testCallStr("if")
        |""".stripMargin ->
        IfTag(neq(LiteralRaw.quote("a"), LiteralRaw.quote("b"))).wrap(
          testServiceCallStr("if")
        ),
      """
        |    try:
        |       Test.testCallStr("try")
        |""".stripMargin ->
        TryTag.wrap(
          testServiceCallStr("try")
        )
    )

    (1 to tests.length)
      .map(tests.take(_))
      .flatMap(test => ifTry.map(test -> _))
      .foreach { case (test, (ifOrTry, tag)) =>
        val script = testServiceDef +
          """
            |func test():           
            """.stripMargin +
          ifOrTry +
          test.map(n => s"    par Test.testCallStr(\"$n\")\n").mkString

        insideBody(script) { body =>
          val expected = ParTag.wrap(
            tag +: test.map(n => testServiceCallStr(n))
          )

          body.equalsOrShowDiff(expected) should be(true)
        }
      }
  }

  it should "forbid else without if" in {
    val scriptTry = testServiceDef +
      """
        |func test():           
        |    try:
        |       Test.testCallStr("try")
        |    else:
        |       Test.testCallStr("else")
        |""".stripMargin

    val scriptSingle = testServiceDef +
      """
        |func test():           
        |    else:
        |       Test.testCallStr("else")
        |""".stripMargin

    insideSemErrors(scriptTry) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

    insideSemErrors(scriptSingle) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  it should "forbid catch without try" in {
    val scriptIf = testServiceDef +
      """
        |func test():           
        |    if 1 != 2:
        |       Test.testCallStr("if")
        |    catch e:
        |       Test.testCallStr("catch")
        |""".stripMargin

    val scriptSingle = testServiceDef +
      """
        |func test():           
        |    catch e:
        |       Test.testCallStr("catch")
        |""".stripMargin

    insideSemErrors(scriptIf) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

    insideSemErrors(scriptSingle) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  it should "forbid otherwise without previous instruction" in {
    val scriptOtherwise = testServiceDef +
      """
        |func test():           
        |    otherwise:
        |       Test.testCallStr("otherwise")
        |""".stripMargin

    insideSemErrors(scriptOtherwise) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  it should "forbid par without previous instruction" in {
    val scriptOtherwise = testServiceDef +
      """
        |func test():           
        |    par Test.testCallStr("par")
        |""".stripMargin

    insideSemErrors(scriptOtherwise) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  it should "handle complex cases" in {
    val script = testServiceDef +
      """
        |func test():           
        |    if "a" != "b":
        |       Test.testCallStr("if")
        |    otherwise:
        |       Test.testCallStr("otherwise1")
        |    par Test.testCallStr("par1")
        |    otherwise:
        |       Test.testCallStr("otherwise2")
        |    par Test.testCallStr("par2")
        |    par Test.testCallStr("par3")
        |""".stripMargin

    insideBody(script) { body =>
      val expected = ParTag.wrap(
        TryTag.wrap(
          ParTag.wrap(
            TryTag.wrap(
              IfTag(neq(LiteralRaw.quote("a"), LiteralRaw.quote("b"))).wrap(
                testServiceCallStr("if")
              ),
              testServiceCallStr("otherwise1")
            ),
            testServiceCallStr("par1")
          ),
          testServiceCallStr("otherwise2")
        ),
        testServiceCallStr("par2"),
        testServiceCallStr("par3")
      )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "restrict streams inside `if`" in {
    val script = """
                   |func test():           
                   |   if "a" != "b":
                   |      stream: *string
                   |      stream <<- "a"
                   |   else:
                   |      stream: *string
                   |      stream <<- "b"
                   |""".stripMargin

    insideBody(script) { body =>
      val expected = IfTag(neq(LiteralRaw.quote("a"), LiteralRaw.quote("b"))).wrap(
        declareStreamPush("stream", "a"),
        declareStreamPush("stream", "b")
      )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "restrict streams inside `try`" in {
    val script = """
                   |func test():           
                   |   try:
                   |      stream: *string
                   |      stream <<- "a"
                   |   catch e:
                   |      stream: *string
                   |      stream <<- "b"
                   |   otherwise:
                   |      stream: *string
                   |      stream <<- "c"
                   |""".stripMargin

    insideBody(script) { body =>
      val expected = TryTag.wrap(
        declareStreamPush("stream", "a"),
        SeqTag.wrap(
          AssignmentTag(ValueRaw.error, "e").leaf,
          declareStreamPush("stream", "b")
        ),
        declareStreamPush("stream", "c")
      )

      body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "generate right model for `parseq`" in {
    val script =
      testServiceDef + """
                         |data Peer:
                         |    peer: string
                         |    relay: string
                         |
                         |func test():
                         |   peers = [Peer(peer="a", relay="b"), Peer(peer="c", relay="d")]
                         |   parseq p <- peers on p.peer via p.relay:
                         |      Test.testCallStr(p.peer)
                         |""".stripMargin

    insideBody(script) { body =>
      matchSubtree(body) { case (ForTag("p", _, ForTag.Mode.ParMode), forTag) =>
        matchChildren(forTag) { case (ParTag, parTag) =>
          matchChildren(parTag)(
            { case (OnTag(_, _, strat), _) =>
              strat shouldBe Some(OnTag.ReturnStrategy.Relay)
            },
            { case (NextTag("p"), _) => }
          )
        }
      }
    }
  }

  it should "generate right model for `for ... rec`" in {
    val script = """
                   |func test():
                   |   stream: *i32
                   |   for i <- stream rec:
                   |      stream <<- i
                   |""".stripMargin

    insideBody(script) { body =>
      matchSubtree(body) { case (ForTag("i", stream, ForTag.Mode.RecMode), forTag) =>
        stream.`type` shouldBe StreamType(ScalarType.i32)
        matchChildren(forTag) { case (ParTag, parTag) =>
          matchChildren(parTag)(
            { case (PushToStreamTag(VarRaw(varName, _), Call.Export(streamName, _, _)), _) =>
              varName shouldBe "i"
              streamName shouldBe "stream"
            },
            { case (NextTag("i"), _) => }
          )
        }
      }
    }
  }

  it should "forbid abilities or streams in struct fields" in {
    val scriptAbility =
      """
        |ability Ab:
        |    a: string
        |
        |data St:
        |    a: Ab
        |""".stripMargin

    val scriptStream =
      """
        |data St:
        |    s: *i8
        |""".stripMargin

    insideSemErrors(scriptAbility) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

    insideSemErrors(scriptStream) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  it should "forbid not arrow calls after <-" in {
    def scriptPush(prefix: String, what: String) =
      s"""
         |func main() -> []string:
         |  stream: *string
         |${prefix.split("\n").map("  " + _).mkString("\n")}
         |  stream <- $what
         |  <- stream
         |""".stripMargin

    val scriptLiteral = scriptPush("", "\"a\"")

    insideSemErrors(scriptLiteral) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

    val scriptVar = scriptPush(
      """
        |variable = "value"
        |""".stripMargin,
      "variable"
    )

    insideSemErrors(scriptVar) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

    val scriptArrayElement = scriptPush(
      """
        |arr = ["a", "b", "c"]
        |""".stripMargin,
      "arr[0]"
    )

    insideSemErrors(scriptArrayElement) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  it should "produce warning on unused call results" in {
    val script = """|func test() -> string, string:
                    |  stream: *string
                    |  stream <<- "a"
                    |  stream <<- "b"
                    |  <- stream[0], stream[1]
                    |
                    |func main() -> string:
                    |  a <- test()
                    |  <- a
                    |""".stripMargin

    insideResult(script) { case (warnings, Right(_)) =>
      warnings.exists(_.hints.exists(_.contains("used"))) should be(true)
    }
  }

  {
    val fieldCases = List(
      "field = 42" -> "field = field",
      "field = 42" -> "field",
      "integer = 42" -> "field = integer",
      "" -> "field = 42"
    )

    val strCases = List(
      "str = \"str\"" -> "str = str",
      "str = \"str\"" -> "str",
      "string = \"str\"" -> "str = string",
      "" -> "str = \"str\""
    )

    it should "handle struct creation" in {
      for {
        fieldCase <- fieldCases
        (fieldDef, fieldArg) = fieldCase
        strCase <- strCases
        (strDef, strArg) = strCase
      } {
        val defs = List(fieldDef, strDef).filter(_.nonEmpty).mkString("\n  ")
        val args = List(fieldArg, strArg).filter(_.nonEmpty).mkString(", ")
        val script = s"""|data Struct:
                         |  field: i8
                         |  str: string
                         |
                         |func main() -> Struct:
                         |  $defs
                         |  <- Struct($args)
                         |""".stripMargin

        insideBody(script) { body =>
          matchSubtree(body) { case (ReturnTag(vals), _) =>
            inside(vals.head) { case MakeStructRaw(fields, _) =>
              fields.contains("field") should be(true)
              fields.contains("str") should be(true)
            }
          }
        }
      }
    }

    it should "handle ability creation" in {
      def arrow(name: String) =
        s"""|$name = (x: i8) -> bool:
            |    <- x > 0
            |""".stripMargin
      val arrowCases = List(
        arrow("arrow") -> "arrow = arrow",
        arrow("arrow") -> "arrow",
        arrow("closure") -> "arrow = closure"
      )

      for {
        arrowCase <- arrowCases
        (arrowDef, arrowArg) = arrowCase
        fieldCase <- fieldCases
        (fieldDef, fieldArg) = fieldCase
        strCase <- strCases
        (strDef, strArg) = strCase
      } {
        val defs = List(arrowDef, fieldDef, strDef).filter(_.nonEmpty).mkString("\n  ")
        val args = List(arrowArg, fieldArg, strArg).filter(_.nonEmpty).mkString(", ")
        val script = s"""|ability Ab:
                         |  field: i8
                         |  str: string
                         |  arrow(x: i8) -> bool
                         |
                         |func main() -> Ab:
                         |  $defs
                         |  <- Ab($args)
                         |""".stripMargin

        insideBody(script) { body =>
          matchSubtree(body) { case (ReturnTag(vals), _) =>
            inside(vals.head) { case AbilityRaw(fields, _) =>
              fields.contains("arrow") should be(true)
              fields.contains("field") should be(true)
              fields.contains("str") should be(true)
            }
          }
        }
      }
    }
  }

  it should "forbid duplicate fields in data or ability creation" in {
    List("data", "ability").foreach { form =>

      val script = s"""|$form StructOrAb:
                       |  field: i8
                       |
                       |func main() -> StructOrAb:
                       |  field = 24
                       |  <- StructOrAb(field = 42, field)
                       |""".stripMargin

      insideSemErrors(script) { errors =>
        atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
      }
    }
  }

  it should "forbid duplicate fields in data copy" in {

    val script = """|data Struct:
                    |  field: i8
                    |
                    |func main() -> Struct:
                    |  st = Struct(field = 24)
                    |  field = 37
                    |  <- st.copy(field = 42, field)
                    |""".stripMargin

    insideSemErrors(script) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

  }

  it should "report an error on unknown service methods" in {
    val script = """
                   |service Test("test"):
                   |  call(i: i32) -> i32
                   |
                   |func test():
                   |  Test.unknown("test")
                   |""".stripMargin

    insideSemErrors(script) { errors =>
      errors.toChain.toList.exists {
        case RulesViolated(_, messages) =>
          messages.exists(_.contains("not defined")) &&
          messages.exists(_.contains("unknown"))
        case _ => false
      }
    }
  }

  it should "report an error on unknown ability arrows" in {
    val script = """
                   |ability Test:
                   |  call(i: i32) -> i32
                   |
                   |func test():
                   |  call = (i: i32) -> i32:
                   |    <- i
                   |
                   |  t = Test(call)
                   |    
                   |  t.unknown("test")
                   |""".stripMargin

    insideSemErrors(script) { errors =>
      errors.toChain.toList.exists {
        case RulesViolated(_, messages) =>
          messages.exists(_.contains("not defined")) &&
          messages.exists(_.contains("unknown"))
        case _ => false
      }
    }
  }

  it should "allow pushing `nil` to stream" in {
    def test(quantifier: String) = {
      val script = s"""
                      |func test() -> []${quantifier}string:
                      |  stream: *${quantifier}string
                      |  stream <<- nil
                      |  <- stream
                      |""".stripMargin

      insideBody(script) { body =>
        matchSubtree(body) { case (PushToStreamTag(VarRaw(name, _), _), _) =>
          name shouldEqual "nil"
        }
      }
    }

    test("?")
    test("[]")
  }

  it should "allow putting stream into collection" in {
    def test(t: String, p: String) = {
      val script = s"""
                      |service Srv("test-srv"):
                      |  consume(value: ${t}[]string)
                      |
                      |func test():
                      |  stream: *string
                      |  Srv.consume(${p}[stream])
                      |""".stripMargin

      insideBody(script) { body =>
        matchSubtree(body) {
          case (CallArrowRawTag(_, ApplyPropertyRaw(_, IntoArrowRaw("consume", _, args))), _) =>
            inside(args) { case (c: CollectionRaw) :: Nil =>
              c.values.exists {
                case VarRaw(name, _) => name == "stream"
                case _ => false
              } should be(true)
            }
        }
      }
    }

    test("[]", "")
    test("?", "?")
  }

  it should "allow `nil` in place of an array or an option" in {
    def test(p: String) = {
      val script = s"""
                      |func length(col: ${p}string) -> u32:
                      |  <- col.length
                      |
                      |func return() -> ${p}string:
                      |  <- nil
                      |
                      |func test() -> u32:
                      |  l <- length(nil)
                      |  n <- return()
                      |  <- l + n.length
                      |""".stripMargin

      insideBody(script, "test".some) { body =>
        matchSubtree(body) {
          case (CallArrowRawTag(_, ca: CallArrowRaw), _) if ca.name == "length" =>
            ca.arguments.length shouldEqual 1
        }
        matchSubtree(body) {
          case (CallArrowRawTag(_, ca: CallArrowRaw), _) if ca.name == "return" =>
            ca.arguments.length shouldEqual 0
        }
      }
    }

    test("[]")
    test("?")
  }

  it should "forbid `nil` in place of a stream" in {
    val scriptAccept = s"""
                          |func length(col: *string) -> u32:
                          |  <- col.length
                          |
                          |func test() -> u32:
                          |  <- length(nil)
                          |""".stripMargin

    val scriptReturn = s"""
                          |func return() -> *string:
                          |  <- nil
                          |
                          |func test() -> u32:
                          |  n <- return()
                          |  <- n.length
                          |""".stripMargin

    insideSemErrors(scriptAccept) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }

    insideSemErrors(scriptReturn) { errors =>
      atLeast(1, errors.toChain.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }
}
