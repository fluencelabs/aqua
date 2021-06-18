package aqua.semantics

import aqua.Node._
import aqua.model.transform._
import aqua.model.{AquaContext, LiteralModel}
import aqua.parser.Ast
import aqua.parser.lift.{LiftParser, Span}
import aqua.types.LiteralType
import aqua.{AquaSpec, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticsSpec extends AnyFlatSpec with Matchers with AquaSpec {

  // use it to fix https://github.com/fluencelabs/aqua/issues/90
  ignore should "create right model" in {
    implicit val fileLift: LiftParser[Span.F] = Span.spanLiftParser

    val script =
      """service A("srv1"):
        |    fn1: -> string
        |
        |func parFunc():           
        |    on "other-peer":
        |        A.fn1()       
        |    par A.fn1()""".stripMargin

    val ast = Ast.fromString(script).toList.head

    val ctx = AquaContext.blank
    val bc = BodyConfig()
    import bc.aquaContextMonoid

    val p = Semantics.process(ast, ctx)

    val func = p.toList.head.funcs("parFunc")

    val proc = Node.cofToNode(func.body.tree)

    val expected =
      seq(
        par(
          on(LiteralModel("\"other-peer\"", LiteralType.string), Nil, callLiteral(1)),
          callLiteral(1)
        )
      )

    println(expected)

    proc.equalsOrPrintDiff(expected) should be(true)

  }
}
