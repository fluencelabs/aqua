package aqua.semantics

import aqua.raw.RawContext
import aqua.parser.Ast
import aqua.raw.ops.{Call, CallServiceTag, FuncOp, OnTag, ParTag, RawTag, SeqTag}
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.types.LiteralType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.~>
import cats.data.Chain

class SemanticsSpec extends AnyFlatSpec with Matchers {

  val emptyCall = Call(Nil, Nil)

  // use it to fix https://github.com/fluencelabs/aqua/issues/90
  "sem" should "create right model" in {
    implicit val fileLift: LiftParser[Span.S] = Span.spanLiftParser
    val parser = Parser.parse(Parser.spanParser)

    val script =
      """service A("srv1"):
        |    fn1: -> string
        |
        |func parFunc():           
        |    on "other-peer":
        |        A.fn1()       
        |    par A.fn1()""".stripMargin

    val ast = parser(script).toList.head

    val ctx = RawContext.blank

    val p = Semantics.process(ast, ctx)(RawContext.implicits(ctx).rawContextMonoid)

    val func = p.toList.head.funcs("parFunc")

    val proc = func.arrow.body

    val expected =
      ParTag.wrap(
        OnTag(
          LiteralRaw("\"other-peer\"", LiteralType.string),
          Chain.empty
        ).wrap(
          CallServiceTag(LiteralRaw.quote("srv1"), "fn1", emptyCall).leaf
        ),
        CallServiceTag(LiteralRaw.quote("srv1"), "fn1", emptyCall).leaf
      )

    proc.equalsOrShowDiff(expected) should be(true)

  }
}
