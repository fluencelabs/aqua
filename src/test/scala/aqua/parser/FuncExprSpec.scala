package aqua.parser

import aqua.Utils
import aqua.parser.Ast.Tree
import aqua.parser.expr.{AbilityIdExpr, CallArrowExpr, FuncExpr, IfExpr}
import aqua.parser.lexer.{ArrowTypeToken, BasicTypeToken, EqOp}
import aqua.semantics.ScalarType.{bool, u64}
import cats.{Eval, Id}
import cats.syntax.traverse._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.semantics.Semantics.{folder, Alg}
import cats.data.Chain
import cats.free.Cofree

import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._
  import aqua.semantics.ScalarType.{string, u32}

  "func header" should "parse" in {
    funcExpr("func some() -> bool") should be(
      FuncExpr("some", List(), Some(bool: BasicTypeToken[Id]), None)
    )
    funcExpr("func some()") should be(FuncExpr("some", List(), None, None))

    val arrowToken =
      ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32)), Some(BasicTypeToken[Id](bool)))
    funcExpr("func some(peer: PeerId, other: u32 -> bool)") should be(
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

  def headTail(
    tree: Cofree[Chain, Expr[Id]],
    headCheck: Expr[Id],
    lengthCheck: Int
  ): Chain[Cofree[Chain, Expr[Id]]] = {
    tree.head should be(headCheck)
    val tail = tree.tailForced
    tail.length should be(lengthCheck)
    tail
  }

  "func expr" should "parse on x: y" in {
    val script =
      """func a():
        | if peer.id == other:
        |  x <- Ab.func()
        |  Peer "some id"
        |  call(true)""".stripMargin

    val tree = FuncExpr.ast[Id](Indent()).parseAll(script).value
    val funcBody = headTail(tree, FuncExpr("a", Nil, None, None), 1).toList
    println("body: " + funcBody)

    val ifBody =
      headTail(
        funcBody.head,
        IfExpr(toVarLambda("peer", List("id")), EqOp[Id](true), toVar("other")),
        3
      ).toList

    ifBody.head.head should be(CallArrowExpr(Some(toName("x")), Some(toAb("Ab")), "func", Nil))
    ifBody(1).head should be(AbilityIdExpr(toAb("Peer"), toStr("some id")))
    ifBody(2).head should be(CallArrowExpr(None, None, "call", List(toBool(true))))
  }
  /*

    "function" should "parse getTime as a whole" in {
      val func =
        """func getTime(peer: PeerId, ret: u32 -> ()) -> string:
          | on peer:
          |   Peer "peer"
          |   t <- Peer.timestamp()
          | ret(t)""".stripMargin

      DefFunc.`deffunc`.parseAll(func).right.value should be(
        DefFunc[Id, HNil](
          getTimeHead,
          NonEmptyList.of(
            On[Id, HNil](
              VarLambda[Id]("peer", Nil),
              NonEmptyList.of(
                AbilityId[Id, HNil]("Peer", Literal[Id]("\"peer\"", LiteralType.string), HNil),
                Extract[Id, HNil]("t", AbilityFuncCall[Id, HNil]("Peer", "timestamp", "Peer.timestamp", Nil, HNil), HNil)
              ),
              HNil
            ),
            FuncCall[Id, HNil]("ret", VarLambda[Id]("t", Nil) :: Nil, HNil)
          ),
          HNil
        )
      )
    }

    "function" should "parse getTime with no return" in {
      val func =
        """func getTime(peer: PeerId, ret: u32 -> ()) -> string:
          | on peer:
          |   Peer "peer"
          |   t <- Peer.timestamp()""".stripMargin

      DefFunc.`deffunc`.parseAll(func).right.value should be(
        DefFunc[Id, HNil](
          getTimeHead,
          NonEmptyList.of(
            On[Id, HNil](
              VarLambda[Id]("peer", Nil),
              NonEmptyList.of(
                AbilityId[Id, HNil]("Peer", Literal[Id]("\"peer\"", LiteralType.string), HNil),
                Extract[Id, HNil]("t", AbilityFuncCall[Id, HNil]("Peer", "timestamp", "Peer.timestamp", Nil, HNil), HNil)
              ),
              HNil
            )
          ),
          HNil
        )
      )
    }*/
}
