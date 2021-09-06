package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import cats.~>

case class AssignmentExpr[F[_]](
  variable: Name[F],
  value: Value[F]
) extends Expr[F](AssignmentExpr, variable) {
  def mapK[K[_]: Comonad](fk: F ~> K): AssignmentExpr[K] = copy(variable.mapK(fk), value.mapK(fk))
}

object AssignmentExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[AssignmentExpr[F]] =
    ((Name.p[F] <* ` = `).with1 ~ Value.`value`).map { case (variable, value) =>
      AssignmentExpr(variable, value)
    }
}
