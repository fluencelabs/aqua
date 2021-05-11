package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.Expr.RootCompanion
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}

case class OnExpr[F[_]](peerId: Value[F], via: List[Value[F]], parPrefix: Option[F[Unit]])
    extends Expr[F] {
  override def root: Boolean = true
}

object OnExpr extends Expr.AndIndented with RootCompanion {

  override def validChildren: List[Expr.Companion] =
    List(
      Expr.defer(OnExpr),
      CallArrowExpr,
      AbilityIdExpr,
      Expr.defer(ForExpr),
      Expr.defer(IfExpr),
      Expr.defer(ElseOtherwiseExpr)
    )

  override def p[F[_]: LiftParser: Comonad]: P[OnExpr[F]] = {
    ((`par`.lift <* ` `).backtrack.?.with1 ~ (`on` *> ` ` *> Value
      .`value`[F] ~ (` ` *> `via` *> ` ` *> Value.`value`[F]).rep0)).map {
      case (parPrefix, (peerId, via)) =>
        OnExpr(peerId, via, parPrefix)
    }
  }
}
