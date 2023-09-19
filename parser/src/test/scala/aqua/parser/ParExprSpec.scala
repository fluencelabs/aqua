package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.*
import aqua.parser.expr.func.{CallArrowExpr, ForExpr, JoinExpr, OnExpr, ParExpr}
import aqua.parser.lexer.{CallArrowToken, Token}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.{Eval, Id}
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree

class ParExprSpec extends AnyFlatSpec with Matchers with Inside with AquaSpec {

  def insidePar(str: String)(testFun: Ast.Tree[Id] => Any) =
    inside(ParExpr.readLine.parseAll(str).map(_.map(_.mapK(spanToId)).forceAll)) {
      case Right(tree) => testFun(tree)
    }

  def par(expr: Expr[Id]): Ast.Tree[Id] =
    Cofree(
      ParExpr(Token.lift(())),
      Eval.now(
        Chain(
          Cofree(
            expr,
            Eval.now(Chain.empty)
          )
        )
      )
    )

  "par" should "be parsed" in {
    insidePar("par x <- y()")(
      _ should be(
        par(
          CallArrowExpr(
            List(toName("x")),
            CallArrowToken(toName("y"), Nil)
          )
        )
      )
    )

    insidePar("par call()")(
      _ should be(
        par(
          CallArrowExpr(
            Nil,
            CallArrowToken(toName("call"), Nil)
          )
        )
      )
    )

    insidePar("par on call() via relay:")(
      _ should be(
        par(
          OnExpr(
            CallArrowToken(toName("call"), Nil),
            toVar("relay") :: Nil
          )
        )
      )
    )

    insidePar("par join call(), x")(
      _ should be(
        par(
          JoinExpr(
            NonEmptyList.of(
              CallArrowToken(toName("call"), Nil),
              toVar("x")
            )
          )
        )
      )
    )

    insidePar("par for w <- getWorkers():")(
      _ should be(
        par(
          ForExpr(
            toName("w"),
            CallArrowToken(toName("getWorkers"), Nil),
            None
          )
        )
      )
    )
  }
}
