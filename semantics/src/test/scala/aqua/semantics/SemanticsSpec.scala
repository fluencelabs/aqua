package aqua.semantics

import aqua.Node
import aqua.Node._
import aqua.model.func.Call
import aqua.model.func.raw.{FuncOp, FuncOps, SeqTag}
import aqua.model.func.resolved.{CallServiceRes, MakeRes}
import aqua.model.transform._
import aqua.model.{AquaContext, LiteralModel, VarModel}
import aqua.parser.Ast
import aqua.parser.lift.{LiftParser, Span}
import aqua.types.{LiteralType, ScalarType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticsSpec extends AnyFlatSpec with Matchers {

  // use it to fix https://github.com/fluencelabs/aqua/issues/90
  "sem" should "create right model" in {
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

  "model" should "have correct path on an error" in {
    implicit val fileLift: LiftParser[Span.F] = Span.spanLiftParser

    val script =
      """service A("srv1"):
        |    fn1: -> string
        |
        |func parFunc():           
        |    on "other-peer":
        |        A.fn1()""".stripMargin

    val ast = Ast.fromString(script).toList.head

    val ctx = AquaContext.blank
    val bc = BodyConfig()
    import bc.aquaContextMonoid

    val p = Semantics.process(ast, ctx)

    val func = p.toList.head.funcs("parFunc")

    val fc = Transform.forClient(func, bc)

    val actual = Node.cofToNode(fc)

    val expected: Node.Res = {
      MakeRes.xor(
        MakeRes.seq(
          MakeRes.leaf(
            CallServiceRes(
              LiteralModel.quote("getDataSrv"),
              "-relay-",
              Call(Nil, Some(Call.Export("-relay-", ScalarType.string))),
              initPeer
            )
          ),
          through(relayV),
          MakeRes.xor(
            callLiteralRes(1, otherPeerL),
            MakeRes.seq(
              through(relayV),
              MakeRes.leaf(
                CallServiceRes(
                  LiteralModel.quote("errorHandlingSrv"),
                  s"error",
                  Call(VarModel.lastError :: LiteralModel("1", LiteralType.number) :: Nil, None),
                  initPeer
                )
              )
            )
          )
        ),
        MakeRes.leaf(
          CallServiceRes(
            LiteralModel.quote("errorHandlingSrv"),
            s"error",
            Call(VarModel.lastError :: LiteralModel("2", LiteralType.number) :: Nil, None),
            initPeer
          )
        )
      )
    }

    Node.equalsOrPrintDiff(actual, expected) should be(true)

  }
}
