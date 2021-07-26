package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{EqOp, Literal, Value}
import aqua.parser.lift.LiftParser
import aqua.types.LiteralType
import cats.Comonad
import cats.parse.{Parser => P}

case class IfExpr[F[_]](left: Value[F], eqOp: EqOp[F], right: Value[F])
    extends Expr[F](IfExpr, eqOp)

object IfExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    Expr.defer(OnExpr) ::
      CallArrowExpr ::
      AbilityIdExpr ::
      AssignmentExpr ::
      PushToStreamExpr ::
      Expr.defer(ParExpr) ::
      Expr.defer(CoExpr) ::
      Expr.defer(TryExpr) ::
      Expr.defer(ForExpr) ::
      Expr.defer(IfExpr) ::
      Expr.defer(ElseOtherwiseExpr) ::
      Expr.defer(CatchExpr) ::
      Nil

  override def p[F[_]: LiftParser: Comonad]: P[IfExpr[F]] =
    (`if` *> ` ` *> Value.`value`[F] ~ (` ` *> EqOp.p[F] ~ (` ` *> Value.`value`[F])).?).map {
      case (left, Some((e, right))) =>
        IfExpr(left, e, right)
      case (left, None) =>
        IfExpr(left, EqOp(left.as(true)), Literal(left.as("true"), LiteralType.bool))
    }
}
