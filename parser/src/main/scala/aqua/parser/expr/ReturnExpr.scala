package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class ReturnExpr[F[_]](value: Value[F]) extends Expr[F]

object ReturnExpr extends Expr.Companion {

  override def p[F[_]: LiftParser: Comonad]: Parser[ReturnExpr[F]] =
    (`<-` *> ` ` *> Value.`value`[F]).map(ReturnExpr(_))
}
