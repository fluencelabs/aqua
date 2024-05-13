package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.ReturnExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.ValueToken
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.{Comonad, ~>}

case class ReturnExpr[F[_]](values: NonEmptyList[ValueToken[F]])
    extends Expr[F](ReturnExpr, values.head) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ReturnExpr[K] =
    copy(values.map(_.mapK(fk)))
}

object ReturnExpr extends Expr.Leaf {

  override val p: Parser[ReturnExpr[Span.S]] =
    (`<-` *> ` ` *> comma(ValueToken.`value`)).map(ReturnExpr(_))
}
