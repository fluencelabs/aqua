package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{ArrowTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class ArrowTypeExpr[F[_]](name: Name[F], `type`: ArrowTypeToken[F]) extends Expr[F]

object ArrowTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[ArrowTypeExpr[F]] =
    ((Name.p[F] <* ` : `) ~ ArrowTypeToken.`arrowdef`[F]).map { case (name, t) =>
      ArrowTypeExpr(name, t)
    }
}
