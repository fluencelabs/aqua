package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser

case class ReturnExpr[F[_]](values: NonEmptyList[Value[F]]) extends Expr[F](ReturnExpr, values.head)

object ReturnExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[ReturnExpr[F]] =
    (`<-` *> ` ` *> comma(Value.`value`[F])).map(ReturnExpr(_))
}
