package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}

case class OnExpr[F[_]](peerId: Value[F], via: List[Value[F]], parPrefix: Option[F[Unit]])
    extends Expr[F](OnExpr) with ParPrefix[F]

object OnExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    Expr.defer(OnExpr) ::
      CallArrowExpr ::
      AbilityIdExpr ::
      Expr.defer(ForExpr) ::
      Expr.defer(IfExpr) ::
      Expr.defer(ElseOtherwiseExpr) ::
      Nil

  override def p[F[_]: LiftParser: Comonad]: P[OnExpr[F]] = {
    (ParPrefix.p.with1 ~ (`on` *> ` ` *> Value
      .`value`[F] ~ (` ` *> `via` *> ` ` *> Value.`value`[F]).rep0)).map {
      case (parPrefix, (peerId, via)) =>
        OnExpr(peerId, via, parPrefix)
    }
  }
}
