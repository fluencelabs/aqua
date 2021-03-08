package aqua.parser.ast

import aqua.parser.lexer.{ArrowName, ArrowTypeToken}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class ArrowTypeExpr[F[_]](name: ArrowName[F], `type`: ArrowTypeToken[F]) extends Expr[F] {}

object ArrowTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    ((ArrowName.an[F] <* ` : `) ~ ArrowTypeToken.`arrowdef`[F]).map {
      case (name, t) => ArrowTypeExpr(name, t)
    }
}
