package aqua.semantics

import aqua.raw.RawContext
import aqua.parser.Ast
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp, OnTag, ParTag, RawTag, SeqGroupTag, SeqTag}
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.types.*
import aqua.raw.ops.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.~>
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.show.*
import cats.data.Validated

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
      val serviceCall =
        CallArrowRawTag.service(LiteralRaw.quote("srv1"), "fn1", emptyCall, "A", arrowType).leaf

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
        IfTag(LiteralRaw.number(1), LiteralRaw.number(2), true).wrap(
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
            AssignmentTag(ValueRaw.LastError, "e").leaf,
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
            AssignmentTag(ValueRaw.LastError, "e").leaf,
            testServiceCallStr("catch1")
          ),
          SeqTag.wrap(
            AssignmentTag(ValueRaw.LastError, "e").leaf,
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
        IfTag(LiteralRaw.number(1), LiteralRaw.number(2), false).wrap(
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
        IfTag(LiteralRaw.quote("a"), LiteralRaw.quote("b"), false).wrap(
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
        IfTag(LiteralRaw.quote("a"), LiteralRaw.quote("b"), false).wrap(
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
              IfTag(LiteralRaw.quote("a"), LiteralRaw.quote("b"), false).wrap(
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
}
