package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}

case class AssignmentExpr[F[_]](
  variable: Name[F],
  value: Value[F]
) extends Expr[F]

object AssignmentExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[AssignmentExpr[F]] =
    ((Name.p[F] <* ` = `).backtrack.with1 ~ Value.`value`).map { case (variable, value) =>
      AssignmentExpr(variable, value)
    }
}
