package aqua.semantics

import aqua.Node
import aqua.Node.*
import aqua.model.AquaContext
import aqua.raw.RawContext
import aqua.parser.Ast
import aqua.raw.ops.{CallServiceTag, FuncOp, FuncOps, OnTag, ParTag, RawTag, SeqTag}
import aqua.model.transform.TransformConfig
import aqua.model.transform.funcop.*
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.types.LiteralType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.~>
import cats.data.Chain

class SemanticsSpec extends AnyFlatSpec with Matchers {

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
    val bc = TransformConfig()
    import bc.rawContextMonoid

    val p = Semantics.process(ast, ctx)

    val func = p.toList.head.funcs("parFunc")

    val proc = Node.cofToNode(func.arrow.body.tree)

    val expected: Node[RawTag] =
      Node.cofToNode(

        SeqTag.wrap(
          ParTag.wrap(
            OnTag(
              LiteralRaw("\"other-peer\"", LiteralType.string),
              Chain.empty
            ).wrap(
              CallServiceTag(LiteralRaw.quote("srv1"), "fn1", Node.emptyCall)
            ),
            CallServiceTag(LiteralRaw.quote("srv1"), "fn1", Node.emptyCall).leaf
          )
        )
      )

    proc.equalsOrPrintDiff(expected) should be(true)

  }
}
