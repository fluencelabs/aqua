package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.comonad._

case class ForExpr[F[_]](
  item: Name[F],
  iterable: Value[F],
  mode: Option[(F[ForExpr.Mode], ForExpr.Mode)]
) extends Expr[F](ForExpr, item)

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
      Expr.defer(TryExpr) ::
      Expr.defer(IfExpr) ::
      Expr.defer(ElseOtherwiseExpr) ::
      Expr.defer(CatchExpr) ::
      Nil

  override def p[F[_]: LiftParser: Comonad]: P[ForExpr[F]] =
    ((`for` *> ` ` *> Name.p[F] <* ` <- `) ~ Value
      .`value`[F] ~ (` ` *> (`par`.as(ParMode: Mode).lift | `try`.as(TryMode: Mode).lift)).?).map {
      case ((item, iterable), mode) =>
        ForExpr(item, iterable, mode.map(m => m -> m.extract))
    }
}
