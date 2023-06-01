package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.*
import aqua.parser.expr.func.{AbilityIdExpr, ArrowExpr, CallArrowExpr, IfExpr, OnExpr, ReturnExpr}
import aqua.parser.lexer.{
  ArrowTypeToken,
  BasicTypeToken,
  CallArrowToken,
  EqOp,
  LiteralToken,
  Token,
  VarToken
}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.ScalarType.*
import cats.Id
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.syntax.foldable.*
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{Inside, Inspectors}
import org.scalatest.matchers.should.Matchers
import cats.~>

import scala.collection.mutable
import scala.language.implicitConversions
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

class FuncExprSpec extends AnyFlatSpec with Matchers with Inside with Inspectors with AquaSpec {
  import AquaSpec._

  private val parser = Parser.spanParser

  "func header" should "parse" in {
    funcExpr("func some") should be(
      FuncExpr[Id](toName("some"))
    )

    val arrowToken =
      ArrowTypeToken[Id]((), List(None -> BasicTypeToken[Id](u8)), List(BasicTypeToken[Id](bool)))
    arrowExpr("(peer: PeerId, other: u8 -> bool)") should be(
      ArrowExpr[Id](
        toNamedArrow(("peer" -> toNamedType("PeerId")) :: ("other" -> arrowToken) :: Nil, Nil)
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
        toNamedArrow(("peer" -> toNamedType("PeerId")) :: ("other" -> arrowToken2) :: Nil, Nil)
      )
    )

    val arrowToken3 = ArrowTypeToken[Id]((), List(None -> BasicTypeToken[Id](u32)), Nil)
    arrowExpr("(peer: PeerId, ret: u32 -> ()) -> string, u32") should be(
      ArrowExpr[Id](
        toNamedArrow(
          ("peer" -> toNamedType("PeerId")) :: ("ret" -> arrowToken3) :: Nil,
          BasicTypeToken[Id](string) :: BasicTypeToken[Id](u32) :: Nil
        )
      )
    )
  }

  def checkHeadGetTail(
    tree: Cofree[Chain, Expr[Span.S]],
    headCheck: Expr[Id],
    lengthCheck: Int
  ): Chain[Cofree[Chain, Expr[Span.S]]] = {
    tree.head.mapK(nat) should be(headCheck)
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

    val tree = FuncExpr.ast.parseAll(script).value.toEither.value
    val funcBody = checkHeadGetTail(tree, FuncExpr(toName("a")), 1).toList

    val arrowExpr =
      checkHeadGetTail(funcBody.head, ArrowExpr[Id](toNamedArrow(Nil, Nil)), 1).toList

    val ifBody =
      checkHeadGetTail(
        arrowExpr.head,
        IfExpr(toVarLambda("peer", List("id")), EqOp[Id](true), toVar("other")),
        3
      ).toList

    ifBody.head.head.mapK(spanToId) should be(
      CallArrowExpr(List(toName("x")), CallArrowToken(Some(toNamedType("Ab")), "func", Nil))
    )
    ifBody(1).head.mapK(spanToId) should be(AbilityIdExpr(toNamedType("Peer"), toStr("some id")))
    ifBody(2).head.mapK(spanToId) should be(
      CallArrowExpr(Nil, CallArrowToken(None, "call", List(toBool(true))))
    )

  }

  "function with mixed blocks indent" should "parse without error" in {
    val scriptFor =
      """func try():
        | v <- Local.call()
        | for x <- v:
        |   foo(x)
        |   for y <- x:
        |    bar(y)
        | for z <- v:
        |     baz(z)
        |""".stripMargin

    val scriptIf =
      """func try(v: bool):
        |  if v:
        |    foo()
        |  else:
        |   if v:
        |       bar()
        |   else:
        |     baz()
        |""".stripMargin

    val scriptOn =
      """func try():
        |  on "some" via "some":
        |    foo()
        |    on "some" via "some":
        |     bar()
        |  on "some" via "some":
        |     bar()
        |""".stripMargin

    forAll(List(scriptFor, scriptIf, scriptOn)) { script =>
      parser.parseAll(script).value.toEither.isRight shouldBe true
    }
  }

  "function with wrong indent" should "parse with error" in {
    val scriptSimple =
      """func try():
        |  v <- Local.call()
        | x = v
        |""".stripMargin

    val scriptReturn =
      """func try() -> bool:
        |   v <- Local.call()
        |  <- v
        |""".stripMargin

    val scriptFor =
      """func try():
        | v <- call()
        |  for x <- v:
        |   foo(x)
        |""".stripMargin

    val scriptIf =
      """func try(v: bool):
        |   if v:
        |    foo()
        |  call()
        |""".stripMargin

    val scriptOn =
      """func try():
        |   call()
        |  on "some" via "some":
        |   foo()
        |""".stripMargin

    forAll(List(scriptSimple, scriptReturn, scriptFor, scriptIf, scriptOn)) { script =>
      inside(parser.parseAll(script).value) { case Invalid(errors) =>
        forAll(errors.toList) { error =>
          inside(error) { case BlockIndentError(_, message) =>
            message shouldEqual "Inconsistent indentation in the block"
          }
        }
      }
    }
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
      acc.enqueue(tag.mapK(nat))
    }

    qTree.d() shouldBe RootExpr(Token.lift[Id, Unit](()))
    // Local service
    qTree.d() shouldBe ServiceExpr(toNamedType("Local"), Some(toStr("local")))
    qTree.d() shouldBe ArrowTypeExpr("gt", toArrowType(Nil, Some(scToBt(bool))))
    qTree.d() shouldBe FuncExpr(
      "tryGen"
    )
    qTree.d() shouldBe ArrowExpr(toArrowType(Nil, Some(scToBt(bool))))
    qTree.d() shouldBe OnExpr(toStr("deeper"), List(toStr("deep")))
    qTree.d() shouldBe CallArrowExpr(
      List("v"),
      CallArrowToken(Some(toNamedType("Local")), "gt", Nil)
    )
    qTree.d() shouldBe ReturnExpr(NonEmptyList.one(toVar("v")))
    // genC function
    qTree.d() shouldBe FuncExpr(
      "genC"
//      toNamedArrow(("val" -> string) :: Nil, boolSc :: Nil),
//      List("two": VarLambda[Id])
    )
    qTree.d() shouldBe ArrowExpr(toNamedArrow(("val" -> string) :: Nil, boolSc :: Nil))
    qTree.d() shouldBe CallArrowExpr(
      List("one"),
      CallArrowToken(Some(toNamedType("Local")), "gt", List())
    )
    qTree.d() shouldBe OnExpr(toStr("smth"), List(toStr("else")))
    qTree.d() shouldBe CallArrowExpr(List("two"), CallArrowToken(None, "tryGen", List()))
    qTree.d() shouldBe CallArrowExpr(
      List("three"),
      CallArrowToken(Some(toNamedType("Local")), "gt", List())
    )
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
