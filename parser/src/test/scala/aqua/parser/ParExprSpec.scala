package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.{CallArrowExpr, ParExpr}
import aqua.parser.lexer.{CallArrowToken, Token}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.{Eval, Id}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.data.Chain
import cats.free.Cofree

class ParExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec._

  "par" should "be parsed" in {
    ParExpr.readLine.parseAll("par x <- y()").value.map(_.mapK(spanToId)).forceAll should be(
      Cofree[Chain, Expr[Id]](
        ParExpr[Id](Token.lift[Id, Unit](())),
        Eval.now(
          Chain(
            Cofree[Chain, Expr[Id]](
              CallArrowExpr(
                List(AquaSpec.toName("x")),
                CallArrowToken(
                  None,
                  AquaSpec.toName("y"),
                  Nil
                )

              ),
              Eval.now(Chain.empty)
            )
          )
        )
      )
    )
  }
}
