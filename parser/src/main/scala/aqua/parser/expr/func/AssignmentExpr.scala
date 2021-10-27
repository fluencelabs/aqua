package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class AssignmentExpr[F[_]](
  variable: Name[F],
  value: Value[F]
) extends Expr[F](AssignmentExpr, variable) {
  def mapK[K[_]: Comonad](fk: F ~> K): AssignmentExpr[K] = copy(variable.mapK(fk), value.mapK(fk))
}

object AssignmentExpr extends Expr.Leaf {

  override val p: P[AssignmentExpr[Span.F]] =
    ((Name.p <* ` = `).with1 ~ Value.`value`).map { case (variable, value) =>
      AssignmentExpr(variable, value)
    }
}
