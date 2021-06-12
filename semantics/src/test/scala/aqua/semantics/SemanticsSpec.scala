package aqua.semantics

import aqua.AquaSpec
import aqua.model.func.Call
import aqua.model.func.body.{CallServiceTag, OnTag, ParTag, SeqTag}
import aqua.model.transform.BodyConfig
import aqua.model.{AquaContext, LiteralModel}
import aqua.parser.Ast
import aqua.parser.lift.{LiftParser, Span}
import aqua.types.LiteralType
import cats.data.Chain
import cats.free.Cofree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SemanticsSpec extends AnyFlatSpec with Matchers with AquaSpec {

  def headTail(tree: Cofree[Chain, OnTag]): (OnTag, Chain[Cofree[Chain, OnTag]]) = {
    (tree.head, tree.tailForced)
  }

  def unpackSeq(tree: Cofree[Chain, OnTag]): (Cofree[Chain, OnTag], Cofree[Chain, OnTag]) = {
    val seqHead = tree.head
    seqHead shouldBe SeqTag
    val seqC = tree.tailForced

    val left = seqC.headOption.get
    val right = seqC.get(1).get

    (left, right)
  }

  "semantics" should "create model" in {
    implicit val fileLift: LiftParser[Span.F] = Span.spanLiftParser

    def lpCall(name: String) =
      CallServiceTag(
        LiteralModel("lp", LiteralType.string),
        "print",
        Call(List(LiteralModel(name, LiteralType.string)), None)
      )

    val script =
      """service LocalPrint("lp"):
        |    print: string -> ()
        |
        |func topologyTest():
        |    on "friend":
        |        str2 <- LocalPrint.print("in on")
        |    par LocalPrint.print("in par")
        |""".stripMargin

    val ast = Ast.fromString(script).toList.head
    val ctx = AquaContext.blank
    val bc = BodyConfig()
    import bc.aquaContextMonoid

    val a = Semantics.process(ast, ctx).toList.head
    println(a)

    val body = a.allFuncs()("topologyTest").body.tree.forceAll

    // here is test on broken logic
    // par is missed, so, if we will have par, is seq will exist?
    // could Seq be with one child? if yes, right part will be (null)?
    val seqHead = body.head
    seqHead shouldBe SeqTag
    val seqC = body.tailForced

    val parBlock = seqC.headOption.get
    println(seqC)
    parBlock.head shouldBe ParTag
    val parC = parBlock.tailForced

    // left par
    val onBlock = parC.headOption.get
    onBlock.head.isInstanceOf[OnTag] should be(true)

    val callInOn = onBlock.tailForced.headOption.get.asInstanceOf[CallServiceTag]
    callInOn should be(lpCall("in on"))

    val rightPar = parC.get(1).get
    val callInPar = rightPar.head.asInstanceOf[CallServiceTag]
    callInPar should be(lpCall("in par"))

    rightPar.tailForced shouldBe empty
  }
}
