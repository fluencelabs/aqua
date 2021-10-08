package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{ForExpr, IfExpr}
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{EqOp, Literal, Value}
import aqua.parser.lift.LiftParser
import aqua.types.LiteralType
import cats.parse.Parser as P
import cats.{Comonad, ~>}

case class IfExpr[F[_]](left: Value[F], eqOp: EqOp[F], right: Value[F])
    extends Expr[F](IfExpr, eqOp) {

  override def mapK[K[_]: Comonad](fk: F ~> K): IfExpr[K] =
    copy(left.mapK(fk), eqOp.mapK(fk), right.mapK(fk))
}

object IfExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ForExpr.validChildren

  override def p[F[_]: LiftParser: Comonad]: P[IfExpr[F]] =
    (`if` *> ` ` *> Value.`value`[F] ~ (` ` *> EqOp.p[F] ~ (` ` *> Value.`value`[F])).?).map {
      case (left, Some((e, right))) =>
        IfExpr(left, e, right)
      case (left, None) =>
        IfExpr(left, EqOp(left.as(true)), Literal(left.as("true"), LiteralType.bool))
    }
}
