package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.{EqOp, Literal, Value}
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.semantics.LiteralType
import cats.Comonad

case class IfExpr[F[_]](left: Value[F], eqOp: EqOp[F], right: Value[F]) extends Expr[F]

object IfExpr extends Expr.AndIndented(OnExpr, ParExpr, CoalgebraExpr, AbilityIdExpr, ForExpr) {

  override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] =
    (`if` *> ` ` *> Value.`value`[F] ~ (` ` *> EqOp.p[F] ~ (` ` *> Value.`value`[F])).? <* ` : \n+`).map {
      case (left, Some((e, right))) =>
        IfExpr(left, e, right)
      case (left, None) =>
        IfExpr(left, EqOp(left.as(true)), Literal(left.as("true"), LiteralType.bool))
    }
}
