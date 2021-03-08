package aqua.parser.ast

import aqua.parser.lexer.{DataTypeToken, Var}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class FieldTypeExpr[F[_]](name: Var[F], `type`: DataTypeToken[F]) extends Expr[F] {}

object FieldTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[FieldTypeExpr[F]] =
    ((Var.p[F] <* ` : `) ~ DataTypeToken.`datatypedef`[F]).map {
      case (name, t) => FieldTypeExpr(name, t)
    }
}
