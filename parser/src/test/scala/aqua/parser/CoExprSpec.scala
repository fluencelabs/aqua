package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.spanToId
import aqua.parser.expr.func.{CallArrowExpr, CoExpr}
import aqua.parser.lexer.{CallArrowToken, Token}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.data.Chain
import cats.free.Cofree
import cats.{Eval, Id}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  "co" should "be parsed" in {
    CoExpr.readLine.parseAll("co x <- y()").value.map(_.mapK(spanToId)).forceAll should be(
      Cofree[Chain, Expr[Id]](
        CoExpr[Id](Token.lift[Id, Unit](())),
        Eval.now(
          Chain(
            Cofree[Chain, Expr[Id]](
              CallArrowExpr(
                List(AquaSpec.toName("x")),
                CallArrowToken(None, AquaSpec.toName("y"), Nil)
              ),
              Eval.now(Chain.empty)
            )
          )
        )
      )
    )
  }
}
