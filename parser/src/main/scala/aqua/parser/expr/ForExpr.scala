package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.Expr.RootCompanion
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}

case class ForExpr[F[_]](
  item: Name[F],
  iterable: Value[F],
  par: Option[F[Unit]],
  parPrefix: Option[F[Unit]]
) extends Expr[F]

object ForExpr extends Expr.AndIndented with RootCompanion {

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
      .`value`[F] ~ (` ` *> `par`.lift).?)).map { case (prefPar, ((item, iterable), par)) =>
      ForExpr(item, iterable, par, prefPar)
    }
}
