package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class FieldTypeExpr[F[_]](name: Name[F], `type`: DataTypeToken[F]) extends Expr[F]

object FieldTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[FieldTypeExpr[F]] =
    ((Name.p[F] <* ` : `) ~ DataTypeToken.`datatypedef`[F]).map { case (name, t) =>
      FieldTypeExpr(name, t)
    }
}
