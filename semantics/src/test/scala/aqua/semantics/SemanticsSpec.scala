package aqua.semantics

import aqua.Node
import aqua.Node.*
import aqua.model.AquaContext
import aqua.raw.RawContext
import aqua.parser.Ast
import aqua.raw.ops.FuncOp
import aqua.model.transform.TransformConfig
import aqua.model.transform.funcop.*
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.raw.ops.{FuncOps, SeqTag}
import aqua.raw.value.LiteralRaw
import aqua.types.LiteralType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.~>

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
    import bc.aquaContextMonoid

    val p = Semantics.process(ast, ctx)

    val func = p.toList.head.funcs("parFunc")

    val proc = Node.cofToNode(func.body.tree)

    val expected: Node.Op =
      FuncOp.wrap(
        SeqTag,
        FuncOps.par(
          on(LiteralRaw("\"other-peer\"", LiteralType.string), Nil, callLiteralRaw(1)),
          callLiteralRaw(1)
        )
      )

    proc.equalsOrPrintDiff(expected) should be(true)

  }
}
