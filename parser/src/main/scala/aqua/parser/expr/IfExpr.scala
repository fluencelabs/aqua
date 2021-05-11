package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.Expr.RootCompanion
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{EqOp, Literal, Value}
import aqua.parser.lift.LiftParser
import aqua.types.LiteralType
import cats.Comonad
import cats.parse.{Parser => P}

case class IfExpr[F[_]](left: Value[F], eqOp: EqOp[F], right: Value[F]) extends Expr[F] {
  override def root: Boolean = true
}

object IfExpr extends Expr.AndIndented with RootCompanion {

  override def validChildren: List[Expr.Companion] =
    List(
      Expr.defer(OnExpr),
      CallArrowExpr,
      AbilityIdExpr,
      Expr.defer(ForExpr),
      Expr.defer(IfExpr),
      Expr.defer(ElseOtherwiseExpr)
    )

  override def p[F[_]: LiftParser: Comonad]: P[IfExpr[F]] =
    (`if` *> ` ` *> Value.`value`[F] ~ (` ` *> EqOp.p[F] ~ (` ` *> Value.`value`[F])).?).map {
      case (left, Some((e, right))) =>
        IfExpr(left, e, right)
      case (left, None) =>
        IfExpr(left, EqOp(left.as(true)), Literal(left.as("true"), LiteralType.bool))
    }
}
