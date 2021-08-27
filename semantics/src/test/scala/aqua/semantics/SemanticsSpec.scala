package aqua.semantics

import aqua.Node
import aqua.Node.*
import aqua.parser.Ast
import aqua.model.func.raw.{FuncOp, FuncOps, SeqTag}
import aqua.model.transform.TransformConfig
import aqua.model.transform.funcop.*
import aqua.model.{AquaContext, LiteralModel}
import aqua.parser.Parser
import aqua.parser.lift.{LiftParser, Span}
import aqua.types.LiteralType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.~>

class SemanticsSpec extends AnyFlatSpec with Matchers {

  // use it to fix https://github.com/fluencelabs/aqua/issues/90
  "sem" should "create right model" in {
    implicit val fileLift: LiftParser[Span.F] = Span.spanLiftParser
    val parser = Parser.parser(Parser.spanParser)

    val script =
      """service A("srv1"):
        |    fn1: -> string
        |
        |func parFunc():           
        |    on "other-peer":
        |        A.fn1()       
        |    par A.fn1()""".stripMargin

    val ast = parser(script).toList.head

    val ctx = AquaContext.blank
    val bc = TransformConfig()
    import bc.aquaContextMonoid

    val p = Semantics.process(ast, ctx)

    val func = p.toList.head.funcs("parFunc")

    val proc = Node.cofToNode(func.body.tree)

    val expected: Node.Raw =
      FuncOp.wrap(
        SeqTag,
        FuncOps.par(
          on(LiteralModel("\"other-peer\"", LiteralType.string), Nil, callLiteralRaw(1)),
          callLiteralRaw(1)
        )
      )

    proc.equalsOrPrintDiff(expected) should be(true)

  }
}
