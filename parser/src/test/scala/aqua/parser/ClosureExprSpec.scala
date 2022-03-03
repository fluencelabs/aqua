package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.{FuncExpr, RootExpr}
import aqua.parser.expr.func.{ArrowExpr, CallArrowExpr, ClosureExpr, ReturnExpr}
import aqua.parser.lexer.{Ability, Token, VarToken}
import aqua.types.ScalarType.string
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id
import cats.syntax.foldable.*
import cats.data.NonEmptyList

import scala.collection.mutable

class ClosureExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  val parser = Parser.spanParser

  "closure header" should "parse" in {
    closureExpr("someName =") should be(
      ClosureExpr[Id](toName("someName"))
    )
  }

  "closure" should "parse" in {
    val script = """func f() -> string:
        |  closure = (s: string) -> string:
        |    LocalSrv.inside()
        |    p2Id <- Peer.identify()
        |    <- p2Id
        |  v <- closure("input")
        |  <- v
        |""".stripMargin

    val tree = parser.parseAll(script).value.toEither.value

    val qTree = tree.tree.foldLeft(mutable.Queue.empty[Expr[Id]]) { case (acc, tag) =>
      acc.enqueue(tag.mapK(nat))
    }

    qTree.d() shouldBe RootExpr(Token.lift[Id, Unit](()))
    qTree.d() shouldBe FuncExpr("f")
    qTree.d() shouldBe ArrowExpr(toArrowType(Nil, Some(scToBt(string))))
    qTree.d() shouldBe ClosureExpr("closure")
    qTree.d() shouldBe ArrowExpr(toNamedArrow(("s", scToBt(string)) :: Nil, scToBt(string) :: Nil))
    qTree.d() shouldBe CallArrowExpr(Nil, Some(Ability[Id]("LocalSrv")), toName("inside"), Nil)
    qTree.d() shouldBe CallArrowExpr(toName("p2Id") :: Nil, Some(Ability[Id]("Peer")), toName("identify"), Nil)
    qTree.d() shouldBe ReturnExpr(NonEmptyList(VarToken[Id](toName("p2Id")), Nil))
    qTree.d() shouldBe CallArrowExpr(toName("v") :: Nil, None, toName("closure"), toStr("input") :: Nil)
    qTree.d() shouldBe ReturnExpr(NonEmptyList(VarToken[Id](toName("v")), Nil))
  }
}
