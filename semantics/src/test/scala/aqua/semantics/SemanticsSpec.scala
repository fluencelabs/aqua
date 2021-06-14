package aqua.semantics

import aqua.AquaSpec
import aqua.model.Node.{callLiteral, on, par}
import aqua.model.transform.BodyConfig
import aqua.model.{AquaContext, LiteralModel, Node}
import aqua.parser.Ast
import aqua.parser.lift.{LiftParser, Span}
import aqua.types.LiteralType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticsSpec extends AnyFlatSpec with Matchers with AquaSpec {

  "semantics" should "create right model" in {
    implicit val fileLift: LiftParser[Span.F] = Span.spanLiftParser

    val script =
      """service LocalPrint("srv1"):
        |    fn1: -> ()        
        |
        |func topologyTest():
        |    on "other-peer":
        |        LocalPrint.fn1()
        |    par LocalPrint.fn1()
        |""".stripMargin

    val ast = Ast.fromString(script).toList.head
    val ctx = AquaContext.blank
    val bc = BodyConfig()
    import bc.aquaContextMonoid

    val a = Semantics.process(ast, ctx).toList.head
    println(a)

    val body = a.allFuncs()("topologyTest").body.tree.forceAll

    val node = Node.cofToNode(body)

    val expected =
      par(
        on(LiteralModel("\"other-peer\"", LiteralType.string), Nil, callLiteral(1)),
        callLiteral(1)
      )

    // or
//    val expected =
//      seq(par(on(LiteralModel("\"other-peer\"", LiteralType.string), Nil, callL(1)), callL(1)))

    node should be(expected)
  }
}
