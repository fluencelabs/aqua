package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>

case class FieldTypeExpr[F[_]](name: Name[F], `type`: DataTypeToken[F])
    extends Expr[F](FieldTypeExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): FieldTypeExpr[K] =
    copy(name.mapK(fk), `type`.mapK(fk))
}

object FieldTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[FieldTypeExpr[F]] =
    ((Name.p[F] <* ` : `) ~ DataTypeToken.`datatypedef`[F]).map { case (name, t) =>
      FieldTypeExpr(name, t)
    }
}
