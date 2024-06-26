/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.*
import aqua.parser.expr.func.*
import aqua.parser.lexer.*
import aqua.parser.lift.Span
import aqua.types.ScalarType.*

import cats.data.Chain.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.syntax.foldable.*
import cats.{Eval, Id}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Inside, Inspectors}
import scala.collection.mutable
import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with Inside with Inspectors with AquaSpec {
  import AquaSpec.{given, *}

  private val parser = RootExpr.ast0

  "func header" should "parse" in {
    funcExpr("func some") should be(
      FuncExpr[Id](toName("some"))
    )

    val arrowToken =
      ArrowTypeToken[Id]((), List(None -> ScalarTypeToken[Id](u8)), List(ScalarTypeToken[Id](bool)))
    arrowExpr("(peer: PeerId, other: u8 -> bool)") should be(
      ArrowExpr[Id](
        toNamedArrow(("peer" -> toNamedType("PeerId")) :: ("other" -> arrowToken) :: Nil, Nil)
      )
    )

    val arrowToken2 =
      ArrowTypeToken[Id](
        (),
        List(None -> ScalarTypeToken[Id](u32), None -> ScalarTypeToken[Id](u64)),
        List(ScalarTypeToken[Id](bool))
      )
    arrowExpr("(peer: PeerId, other: u32, u64 -> bool)") should be(
      ArrowExpr[Id](
        toNamedArrow(("peer" -> toNamedType("PeerId")) :: ("other" -> arrowToken2) :: Nil, Nil)
      )
    )

    val arrowToken3 = ArrowTypeToken[Id]((), List(None -> ScalarTypeToken[Id](u32)), Nil)
    arrowExpr("(peer: PeerId, ret: u32 -> ()) -> string, u32") should be(
      ArrowExpr[Id](
        toNamedArrow(
          ("peer" -> toNamedType("PeerId")) :: ("ret" -> arrowToken3) :: Nil,
          ScalarTypeToken[Id](string) :: ScalarTypeToken[Id](u32) :: Nil
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
        IfExpr(equ(toVarLambda("peer", List("id")), toVar("other"))),
        3
      ).toList

    ifBody.head.head.mapK(spanToId) should be(
      CallArrowExpr(
        List(toName("x")),
        PropertyToken[Id](
          VarToken[Id](toName("Ab")),
          NonEmptyList.one(
            IntoArrow[Id](toName("func"), Nil)
          )
        )
      )
    )
    ifBody(1).head.mapK(spanToId) should be(ServiceIdExpr(toNamedType("Peer"), toStr("some id")))
    ifBody(2).head.mapK(spanToId) should be(
      CallArrowExpr(Nil, CallArrowToken("call", List(toBool(true))))
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

    val qTree = tree.foldLeft(mutable.Queue.empty[Expr[Id]]) { case (acc, tag) =>
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
      PropertyToken[Id](
        VarToken[Id](toName("Local")),
        NonEmptyList.one(
          IntoArrow[Id](toName("gt"), Nil)
        )
      )
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
      PropertyToken[Id](
        VarToken[Id](toName("Local")),
        NonEmptyList.one(
          IntoArrow[Id](toName("gt"), Nil)
        )
      )
    )
    qTree.d() shouldBe OnExpr(toStr("smth"), List(toStr("else")))
    qTree.d() shouldBe CallArrowExpr(List("two"), CallArrowToken("tryGen", Nil))
    qTree.d() shouldBe CallArrowExpr(
      List("three"),
      PropertyToken[Id](
        VarToken[Id](toName("Local")),
        NonEmptyList.one(
          IntoArrow[Id](toName("gt"), Nil)
        )
      )
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

  "function with if" should "be parsed correctly" in {
    val script =
      """func ifTest():
        |    if (1 + 1) == 2:
        |       Test.test("one")
        |
        |    if 2 < 3 != (1 > 2):
        |       Test.test("two")
        |""".stripMargin

    inside(parser.parseAll(script).value) { case Valid(ast) =>
      Cofree.cata[Chain, Expr[Span.S], Int](ast)((expr, results) =>
          // Count `if`s inside the tree
          Eval.later(results.sumAll + (expr match {
            case IfExpr(_) => 1
            case _ => 0
          }))
        )
        .value shouldBe 2
    }
  }
}
