package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.*
import aqua.parser.expr.func.{AbilityIdExpr, ArrowExpr, CallArrowExpr, IfExpr, OnExpr, ReturnExpr}
import aqua.parser.lexer.{ArrowTypeToken, BasicTypeToken, EqOp, Literal, Token, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.ScalarType.*
import cats.Id
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.syntax.foldable.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  val parser = Parser.idParser

  "func header" should "parse" in {
    funcExpr("func some") should be(
      FuncExpr[Id](toName("some"))
    )

    val arrowToken =
      ArrowTypeToken[Id]((), List(None -> BasicTypeToken[Id](u8)), List(BasicTypeToken[Id](bool)))
    arrowExpr("(peer: PeerId, other: u8 -> bool)") should be(
      ArrowExpr[Id](
        toNamedArrow(("peer" -> toCustomType("PeerId")) :: ("other" -> arrowToken) :: Nil, Nil)
      )
    )

    val arrowToken2 =
      ArrowTypeToken[Id](
        (),
        List(None -> BasicTypeToken[Id](u32), None -> BasicTypeToken[Id](u64)),
        List(BasicTypeToken[Id](bool))
      )
    arrowExpr("(peer: PeerId, other: u32, u64 -> bool)") should be(
      ArrowExpr[Id](
        toNamedArrow(("peer" -> toCustomType("PeerId")) :: ("other" -> arrowToken2) :: Nil, Nil)
      )
    )

    val arrowToken3 = ArrowTypeToken[Id]((), List(None -> BasicTypeToken[Id](u32)), Nil)
    arrowExpr("(peer: PeerId, ret: u32 -> ()) -> string, u32") should be(
      ArrowExpr[Id](
        toNamedArrow(
          ("peer" -> toCustomType("PeerId")) :: ("ret" -> arrowToken3) :: Nil,
          BasicTypeToken[Id](string) :: BasicTypeToken[Id](u32) :: Nil
        )
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

    val tree = FuncExpr.ast[Id]().parseAll(script).value.toEither.value
    val funcBody = checkHeadGetTail(tree, FuncExpr(toName("a")), 1).toList

    val arrowExpr =
      checkHeadGetTail(funcBody.head, ArrowExpr[Id](toNamedArrow(Nil, Nil)), 1).toList

    val ifBody =
      checkHeadGetTail(
        arrowExpr.head,
        IfExpr(toVarLambda("peer", List("id")), EqOp[Id](true), toVar("other")),
        3
      ).toList

    ifBody.head.head should be(
      CallArrowExpr(List(toName("x")), Some(toAb("Ab")), "func", Nil)
    )
    ifBody(1).head should be(AbilityIdExpr(toAb("Peer"), toStr("some id")))
    ifBody(2).head should be(CallArrowExpr(Nil, None, "call", List(toBool(true))))

  }

  "function with wrong indent" should "parse with error" in {
    val script =
      """func tryGen() -> bool:
        |    on "deeper" via "deep":
        |        v <- Local.gt()
        |  <- v
        |""".stripMargin

    parser.parseAll(script).value.toEither.isLeft shouldBe true
  }

  "function with multiline definitions" should "parse without error" in {
    val script =
      """func tryGen(a: string,
        |            b: string):
        |            Call.call()""".stripMargin

    parser.parseAll(script).isRight shouldBe true
  }

  "function with root expression without children" should "parse with error" in {
    val script =
      """func tryGen() -> bool:
        |    on "deeper" via "deep":
        |    <- v
        |""".stripMargin

    parser.parseAll(script).value.toEither.isLeft shouldBe true
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

    val tree = parser.parseAll(script).value.toEither.value

    val qTree = tree.tree.foldLeft(mutable.Queue.empty[Expr[Id]]) { case (acc, tag) =>
      acc.enqueue(tag)
    }

    qTree.d() shouldBe RootExpr(Token.lift[Id, Unit](()))
    // Local service
    qTree.d() shouldBe ServiceExpr(toAb("Local"), Some(toStr("local")))
    qTree.d() shouldBe ArrowTypeExpr("gt", toArrowType(Nil, Some(scToBt(bool))))
    qTree.d() shouldBe FuncExpr(
      "tryGen"
    )
    qTree.d() shouldBe ArrowExpr(toArrowType(Nil, Some(scToBt(bool))))
    qTree.d() shouldBe OnExpr(toStr("deeper"), List(toStr("deep")))
    qTree.d() shouldBe CallArrowExpr(List("v"), Some(toAb("Local")), "gt", Nil)
    qTree.d() shouldBe ReturnExpr(NonEmptyList.one(toVar("v")))
    // genC function
    qTree.d() shouldBe FuncExpr(
      "genC"
//      toNamedArrow(("val" -> string) :: Nil, boolSc :: Nil),
//      List("two": VarLambda[Id])
    )
    qTree.d() shouldBe ArrowExpr(toNamedArrow(("val" -> string) :: Nil, boolSc :: Nil))
    qTree.d() shouldBe CallArrowExpr(List("one"), Some(toAb("Local")), "gt", List())
    qTree.d() shouldBe OnExpr(toStr("smth"), List(toStr("else")))
    qTree.d() shouldBe CallArrowExpr(List("two"), None, "tryGen", List())
    qTree.d() shouldBe CallArrowExpr(List("three"), Some(toAb("Local")), "gt", List())
    qTree.d() shouldBe ReturnExpr(NonEmptyList.one(toVar("two")))
  }

  "function with par" should "be parsed correctly" in {
    val script =
      """func topologyTest():
        |    on "friend":
        |        str2 <- LocalPrint.print("in on")
        |    par LocalPrint.print("in par")""".stripMargin

    parser.parseAll(script).value.toEither.isRight shouldBe true
  }
}
