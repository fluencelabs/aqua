package aqua.parser

import aqua.AquaSpec
import aqua.parser.Ast.parser
import aqua.parser.expr._
import aqua.parser.lexer.{ArrowTypeToken, BasicTypeToken, EqOp}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.ScalarType._
import cats.Id
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.foldable._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "func header" should "parse" in {
    funcExpr("func some() -> bool") should be(
      FuncExpr("some", List(), Some(bool: BasicTypeToken[Id]), None)
    )
    funcExpr("func some()") should be(FuncExpr("some", List(), None, None))

    val arrowToken =
      ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u8)), Some(BasicTypeToken[Id](bool)))
    funcExpr("func some(peer: PeerId, other: u8 -> bool)") should be(
      FuncExpr(
        toName("some"),
        List(toCustomArg("peer", "PeerId"), toArg("other", arrowToken)),
        None,
        None
      )
    )

    val arrowToken2 =
      ArrowTypeToken[Id](
        (),
        List(BasicTypeToken[Id](u32), BasicTypeToken[Id](u64)),
        Some(BasicTypeToken[Id](bool))
      )
    funcExpr("func some(peer: PeerId, other: u32, u64 -> bool)") should be(
      FuncExpr(
        toName("some"),
        List(toCustomArg("peer", "PeerId"), toArg("other", arrowToken2)),
        None,
        None
      )
    )

    val arrowToken3 = ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32)), None)
    funcExpr("func getTime(peer: PeerId, ret: u32 -> ()) -> string") should be(
      FuncExpr(
        toName("getTime"),
        List(toCustomArg("peer", "PeerId"), toArg("ret", arrowToken3)),
        Some(BasicTypeToken[Id](string)),
        None
      )
    )
  }

  def checkHeadGetTail(
    tree: Cofree[Chain, Expr[Id]],
    headCheck: Expr[Id],
    lengthCheck: Int
  ): Chain[Cofree[Chain, Expr[Id]]] = {
    tree.head should be(headCheck)
    val tail = tree.tailForced
    tail.length should be(lengthCheck)
    tail
  }

  def headTail[T](
    tree: Cofree[Chain, T]
  ): (T, List[Cofree[Chain, T]]) = {
    (tree.head, tree.tailForced.toList)
  }

  "func expr" should "parse on x: y" in {
    val script =
      """func a():
        | if peer.id == other:
        |  x <- Ab.func()
        |  Peer "some id"
        |  call(true)""".stripMargin

    val tree = FuncExpr.ast[Id]().parseAll(script).value.value
    val funcBody = checkHeadGetTail(tree, FuncExpr("a", Nil, None, None), 1).toList

    val ifBody =
      checkHeadGetTail(
        funcBody.head,
        IfExpr(toVarLambda("peer", List("id")), EqOp[Id](true), toVar("other")),
        3
      ).toList

    ifBody.head.head should be(
      CallArrowExpr(Some(toName("x")), Some(toAb("Ab")), "func", Nil, None)
    )
    ifBody(1).head should be(AbilityIdExpr(toAb("Peer"), toStr("some id")))
    ifBody(2).head should be(CallArrowExpr(None, None, "call", List(toBool(true)), None))

  }

  "function with wrong indent" should "parse with error" in {
    val script =
      """func tryGen() -> bool:
        |    on "deeper" via "deep":
        |        v <- Local.gt()         
        |  <- v
        |""".stripMargin

    parser[Id]().parseAll(script).value shouldBe Symbol("left")
  }

  "function with root expression without children" should "parse with error" in {
    val script =
      """func tryGen() -> bool:
        |    on "deeper" via "deep":       
        |    <- v
        |""".stripMargin

    parser[Id]().parseAll(script).value shouldBe Symbol("left")
  }

  "multi function expression" should "parse" in {
    val script =
      """service Local("local"):
        |    gt: -> bool
        |    
        |func tryGen() -> bool:
        |    on "deeper" via "deep":
        |        v <- Local.gt()         
        |    <- v
        |
        |func genC(val: string) -> bool:
        |    one <- Local.gt() 
        |    on "smth" via "else":
        |        two <- tryGen()      
        |    three <- Local.gt() 
        |    <- two""".stripMargin

    val tree = parser[Id]().parseAll(script).value.value

    val qTree = tree.tree.foldLeft(mutable.Queue.empty[Expr[Id]]) { case (acc, tag) =>
      acc.enqueue(tag)
    }

    qTree.d() shouldBe RootExpr()
    // Local service
    qTree.d() shouldBe ServiceExpr(toAb("Local"), Some(toStr("local")))
    qTree.d() shouldBe ArrowTypeExpr("gt", toArrowType(Nil, Some(scToBt(bool))))
    qTree.d() shouldBe FuncExpr("tryGen", Nil, Some(scToBt(bool)), Some("v"))
    qTree.d() shouldBe OnExpr(toStr("deeper"), List(toStr("deep")), None)
    qTree.d() shouldBe CallArrowExpr(Some("v"), Some(toAb("Local")), "gt", Nil, None)
    qTree.d() shouldBe ReturnExpr(toVar("v"))
    // genC function
    qTree.d() shouldBe FuncExpr("genC", List(toArgSc("val", string)), Some(boolSc), Some("two"))
    qTree.d() shouldBe CallArrowExpr(Some("one"), Some(toAb("Local")), "gt", List(), None)
    qTree.d() shouldBe OnExpr(toStr("smth"), List(toStr("else")), None)
    qTree.d() shouldBe CallArrowExpr(Some("two"), None, "tryGen", List(), None)
    qTree.d() shouldBe CallArrowExpr(Some("three"), Some(toAb("Local")), "gt", List(), None)
    qTree.d() shouldBe ReturnExpr(toVar("two"))
    /* TODO this is semantics, not parser test

    val f =
      Semantics.generateModel(tree).toList.head.asInstanceOf[ScriptModel]

    val funcs = f.funcs.toList
    val funcTryGen = funcs.head
    val funcOpTryGen = funcTryGen.body.resolveTopology()

    val qTryGen = funcOpTryGen.tree.foldLeft(mutable.Queue.empty[OpTag]) { case (acc, tag) =>
      acc.enqueue(tag)
    }

    val smth = LiteralModel("\"smth\"")
    val smthOn = OnTag(smth, Nil)
    val smthCall = CallServiceTag(
      LiteralModel("\"op\""),
      "identity",
      emptyCall,
      Some(LiteralModel("\"smth\""))
    )

    val elseL = LiteralModel("\"else\"")
    val elseOn = OnTag(elseL, Nil)
    val elseCall = CallServiceTag(
      LiteralModel("\"op\""),
      "identity",
      emptyCall,
      Some(LiteralModel("\"else\""))
    )

    val deeper = LiteralModel("\"deeper\"")
    val deeperOn = OnTag(deeper, Nil)
    val deeperCall = CallServiceTag(
      LiteralModel("\"op\""),
      "identity",
      emptyCall,
      Some(LiteralModel("\"deeper\""))
    )
    val deep = LiteralModel("\"deep\"")
    val deepOn = OnTag(deep, Nil)
    val deepCall = CallServiceTag(
      LiteralModel("\"op\""),
      "identity",
      emptyCall,
      Some(LiteralModel("\"deep\""))
    )
    val local = LiteralModel("\"local\"")

    // tag that we will go to 'deeper'
    qTryGen.d() shouldBe deeperOn
    // move to 'deep' node
    qTryGen.d() shouldBe deepOn
    qTryGen.d() shouldBe deepCall
    // move to 'deeper' node
    qTryGen.d() shouldBe deeperOn
    qTryGen.d() shouldBe deeperCall
    // a call must be with `on` too, so we need to call 'deeper' after we will go out of the scope
    qTryGen.d() shouldBe CallServiceTag(local, "gt", Call(Nil, Some("v")), Some(deeper))
    qTryGen.d() shouldBe deepOn
    // return to 'deeper' node
    qTryGen.d() shouldBe deeperOn
    qTryGen.d() shouldBe deeperCall
    // return to 'deep' node
    qTryGen.d() shouldBe deepOn
    qTryGen.d() shouldBe deepCall

    val funcGenComplex = f.funcs.toList(1)
    val funcOpGenComplex = funcGenComplex.body.resolveTopology()

    val qGenComplex = funcOpGenComplex.tree.foldLeft(mutable.Queue.empty[OpTag]) {
      case (acc, tag) =>
        acc.enqueue(tag)
    }

    qGenComplex.d() shouldBe SeqTag
    qGenComplex.d() shouldBe CallServiceTag(local, "gt", Call(List(), Some("one")), None)
    // tag that we will go to 'smth'
    qGenComplex.d() shouldBe smthOn
    // move to 'else' node
    qGenComplex.d() shouldBe elseOn
    qGenComplex.d() shouldBe elseCall
    // move to 'smth' node
    qGenComplex.d() shouldBe smthOn
    qGenComplex.d() shouldBe smthCall
    qGenComplex.d() shouldBe CallArrowTag(None, "tryGen", Call(List(), Some("two")))
    qGenComplex.d() shouldBe elseOn
    // return to 'smth' node
    qGenComplex.d() shouldBe smthOn
    qGenComplex.d() shouldBe smthCall
    // return to 'else' node
    qGenComplex.d() shouldBe elseOn
    qGenComplex.d() shouldBe elseCall
    qGenComplex.d() shouldBe CallServiceTag(local, "gt", Call(List(), Some("three")), None)*/
  }
}
