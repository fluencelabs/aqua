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
  parPrefix: Option[F[Unit]],
  mode: Option[(F[ForExpr.Mode], ForExpr.Mode)]
) extends Expr[F] {
  override def root: Boolean = true
}

object ForExpr extends Expr.AndIndented {
  sealed trait Mode
  case object TryMode extends Mode
  case object ParMode extends Mode

  override def validChildren: List[Expr.Companion] = List(
    Expr.defer(OnExpr),
    Expr.defer(ForExpr),
    CallArrowExpr,
    AbilityIdExpr,
    Expr.defer(IfExpr),
    Expr.defer(ElseOtherwiseExpr)
  )

  override def p[F[_]: LiftParser: Comonad]: P[ForExpr[F]] =
    ((`par`.lift <* ` `).backtrack.?.with1 ~ ((`for` *> ` ` *> Name.p[F] <* ` <- `) ~ Value
      .`value`[F] ~ (` ` *> (`par`.as(ParMode: Mode).lift | `try`.as(TryMode: Mode).lift)).?)).map {
      case (prefPar, ((item, iterable), mode)) =>
        ForExpr(item, iterable, prefPar, mode.map(m => m -> m.extract))
    }
}
