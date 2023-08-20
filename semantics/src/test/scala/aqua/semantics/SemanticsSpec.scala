package aqua.semantics

import aqua.raw.RawContext
import aqua.parser.Ast
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp, OnTag, ParTag, RawTag, SeqGroupTag, SeqTag}
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.raw.value.{ApplyBinaryOpRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import aqua.raw.ops.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.~>
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.data.Validated
import cats.free.Cofree
import cats.data.State
import cats.Eval

class SemanticsSpec extends AnyFlatSpec with Matchers with Inside {

  val emptyCall = Call(Nil, Nil)

  implicit val fileLift: LiftParser[Span.S] = Span.spanLiftParser
  val parser = Parser.parse(Parser.spanParser)

  val semantics = new RawSemantics[Span.S]()

  def insideBody(script: String)(test: RawTag.Tree => Any): Unit =
    inside(parser(script)) { case Validated.Valid(ast) =>
      val init = RawContext.blank
      inside(semantics.process(ast, init)) { case Validated.Valid(ctx) =>
        inside(ctx.funcs.headOption) { case Some((_, func)) =>
          test(func.arrow.body)
        }
      }
    }

  def insideSemErrors(script: String)(test: NonEmptyChain[SemanticError[Span.S]] => Any): Unit =
    inside(parser(script)) { case Validated.Valid(ast) =>
      val init = RawContext.blank
      inside(semantics.process(ast, init)) { case Validated.Invalid(errors) =>
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

  def testServiceCallStr(str: String) =
    CallArrowRawTag
      .service(
        serviceId = LiteralRaw.quote("test"),
        fnName = "testCallStr",
        call = Call(LiteralRaw.quote(str) :: Nil, Nil),
        name = "Test",
        arrowType = ArrowType(
          ProductType.labelled(("s" -> ScalarType.string) :: Nil),
          ProductType(ScalarType.string :: Nil)
        )
      )
      .leaf

  def equ(left: ValueRaw, right: ValueRaw): ApplyBinaryOpRaw =
    ApplyBinaryOpRaw(ApplyBinaryOpRaw.Op.Eq, left, right)

  def neq(left: ValueRaw, right: ValueRaw): ApplyBinaryOpRaw =
    ApplyBinaryOpRaw(ApplyBinaryOpRaw.Op.Neq, left, right)

  def declareStreamPush(
    name: String,
    value: String
  ): RawTag.Tree = {
    val streamType = StreamType(ScalarType.string)
    val stream = VarRaw(name, streamType)

    RestrictionTag(stream.name, streamType).wrap(
      SeqTag.wrap(
        DeclareStreamTag(stream).leaf,
        PushToStreamTag(
          LiteralRaw.quote(value),
          Call.Export(name, streamType)
        ).leaf
      )
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
      val serviceCall = CallArrowRawTag
        .service(
          serviceId = LiteralRaw.quote("srv1"),
          fnName = "fn1",
          call = emptyCall,
          name = "A",
          arrowType = arrowType
        )
        .leaf

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
            AssignmentTag(ValueRaw.lastError, "e").leaf,
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
            AssignmentTag(ValueRaw.lastError, "e").leaf,
            testServiceCallStr("catch1")
          ),
          SeqTag.wrap(
            AssignmentTag(ValueRaw.lastError, "e").leaf,
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
          AssignmentTag(ValueRaw.lastError, "e").leaf,
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
      matchSubtree(body) { case (ForTag("p", _, None), forTag) =>
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
}
