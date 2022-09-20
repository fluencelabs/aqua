package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{CollectionToken, Name, ValueToken}
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class AssignmentExpr[F[_]](
  variable: Name[F],
  value: ValueToken[F]
) extends Expr[F](AssignmentExpr, variable) {
  def mapK[K[_]: Comonad](fk: F ~> K): AssignmentExpr[K] = copy(variable.mapK(fk), value.mapK(fk))
}

object AssignmentExpr extends Expr.Leaf {

  override val p: P[AssignmentExpr[Span.S]] =
    ((Name.p <* ` = `).with1 ~ ValueToken.`value`).flatMap { case (variable, value) =>
      value match {
        case CollectionToken(_, values) =>
          if (values.isEmpty)
            P.failWith(
              "Assigning empty array to a variable is prohibited. You can create an array with values (like '[a, b, c]') or use '[]' in place."
            )
          else P.pure(AssignmentExpr(variable, value))
        case _ =>
          P.pure(AssignmentExpr(variable, value))
      }
    }
}
