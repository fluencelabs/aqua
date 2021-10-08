package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.{Comonad, ~>}

case class ForExpr[F[_]](
  item: Name[F],
  iterable: Value[F],
  mode: Option[(F[ForExpr.Mode], ForExpr.Mode)]
) extends Expr[F](ForExpr, item) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ForExpr[K] =
    copy(item.mapK(fk), iterable.mapK(fk), mode.map { case (mF, m) => (fk(mF), m) })
}

object ForExpr extends Expr.AndIndented {
  sealed trait Mode
  case object TryMode extends Mode
  case object ParMode extends Mode

  override def validChildren: List[Expr.Lexem] =
    Expr.defer(OnExpr) ::
      Expr.defer(ForExpr) ::
      CallArrowExpr ::
      AbilityIdExpr ::
      AssignmentExpr ::
      PushToStreamExpr ::
      Expr.defer(TryExpr) ::
      Expr.defer(IfExpr) ::
      Expr.defer(ElseOtherwiseExpr) ::
      Expr.defer(CatchExpr) ::
      Expr.defer(ParExpr) ::
      Expr.defer(CoExpr) ::
      Nil

  override def p[F[_]: LiftParser: Comonad]: P[ForExpr[F]] =
    ((`for` *> ` ` *> Name.p[F] <* ` <- `) ~ Value
      .`value`[F] ~ (` ` *> (`par`.as(ParMode: Mode).lift | `try`.as(TryMode: Mode).lift)).?).map {
      case ((item, iterable), mode) =>
        ForExpr(item, iterable, mode.map(m => m -> m.extract))
    }
}
