package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{CallArrowToken, Name, ValueToken, VarToken}
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}
import cats.{Comonad, ~>}

case class CallArrowExpr[F[_]](
  variables: List[Name[F]],
  // Here `ValueToken` is used to allow
  // a, b <- ServiceOrAbility.call()
  callArrow: ValueToken[F]
) extends Expr[F](CallArrowExpr, callArrow) {

  def mapK[K[_]: Comonad](fk: F ~> K): CallArrowExpr[K] =
    copy(
      variables.map(_.mapK(fk)),
      callArrow.mapK(fk)
    )
}

object CallArrowExpr extends Expr.Leaf {

  override val p: P[CallArrowExpr[Span.S]] = {
    val variables: P0[Option[NonEmptyList[Name[Span.S]]]] =
      (comma(Name.variable) <* ` <- `).backtrack.?

    // TODO:    Restrict to function call only
    //          or allow any expression?
    (variables.with1 ~ ValueToken.value).map { case (variables, token) =>
      CallArrowExpr(variables.toList.flatMap(_.toList), token)
    }
  }

}
