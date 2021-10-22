package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class OnExpr[F[_]](peerId: Value[F], via: List[Value[F]]) extends Expr[F](OnExpr, peerId) {

  override def mapK[K[_]: Comonad](fk: F ~> K): OnExpr[K] =
    copy(peerId.mapK(fk), via.map(_.mapK(fk)))
}

object OnExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    Expr.defer(OnExpr) ::
      CallArrowExpr ::
      AbilityIdExpr ::
      AssignmentExpr ::
      PushToStreamExpr ::
      ParExpr ::
      CoExpr ::
      Expr.defer(TryExpr) ::
      Expr.defer(ForExpr) ::
      Expr.defer(IfExpr) ::
      Expr.defer(ElseOtherwiseExpr) ::
      Expr.defer(CatchExpr) ::
      Nil

  override def p: P[OnExpr[Span.F]] = {
    (`on` *> ` ` *> Value
      .`value` ~ (` ` *> `via` *> ` ` *> Value.`value`).rep0).map { case (peerId, via) =>
      OnExpr(peerId, via)
    }
  }
}
