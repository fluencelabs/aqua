package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.{EqOp, Value}
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.Comonad

case class IfExpr[F[_]](left: Value[F], eqOp: EqOp[F], right: Value[F]) extends Expr[F]

object IfExpr extends Expr.AndIndented(OnExpr, ParExpr, CoalgebraExpr, AbilityIdExpr, ForExpr) {

  override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] =
    (`if` *> ` ` *> Value.`value`[F] ~ EqOp.p[F] ~ Value.`value`[F] <* ` : \n+`).map { case ((left, e), right) =>
      IfExpr(left, e, right)
    }
}
