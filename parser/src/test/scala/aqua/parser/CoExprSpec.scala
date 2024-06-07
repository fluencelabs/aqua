package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.*
import aqua.parser.expr.func.{CallArrowExpr, CoExpr, ForExpr, JoinExpr, OnExpr}
import aqua.parser.lexer.{CallArrowToken, Token}
import aqua.parser.lift.LiftParser.given

import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.{Eval, Id}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoExprSpec extends AnyFlatSpec with Matchers with Inside with AquaSpec {

  def insideCo(str: String)(testFun: Ast.Tree[Id] => Any) =
    inside(CoExpr.readLine.parseAll(str).map(_.map(_.mapK(spanToId)).forceAll)) {
      case Right(tree) => testFun(tree)
    }

  def co(expr: Expr[Id]): Ast.Tree[Id] =
    Cofree(
      CoExpr(Token.lift(())),
      Eval.now(
        Chain(
          Cofree(
            expr,
            Eval.now(Chain.empty)
          )
        )
      )
    )

  "co" should "be parsed" in {
    insideCo("co x <- y()")(
      _ should be(
        co(
          CallArrowExpr(
            List(toName("x")),
            CallArrowToken(toName("y"), Nil)
          )
        )
      )
    )

    insideCo("co call()")(
      _ should be(
        co(
          CallArrowExpr(
            Nil,
            CallArrowToken(toName("call"), Nil)
          )
        )
      )
    )

    insideCo("co on call() via relay:")(
      _ should be(
        co(
          OnExpr(
            CallArrowToken(toName("call"), Nil),
            toVar("relay") :: Nil
          )
        )
      )
    )

    insideCo("co join call(), x")(
      _ should be(
        co(
          JoinExpr(
            NonEmptyList.of(
              CallArrowToken(toName("call"), Nil),
              toVar("x")
            )
          )
        )
      )
    )

    insideCo("co for w <- getWorkers():")(
      _ should be(
        co(
          ForExpr(
            Right(toName("w")),
            CallArrowToken(toName("getWorkers"), Nil),
            None
          )
        )
      )
    )
  }
}
