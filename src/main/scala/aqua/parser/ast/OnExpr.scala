package aqua.parser.ast

import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import cats.Comonad

case class OnExpr[F[_]](peerId: Value[F]) extends Expr[F]

object OnExpr extends Expr.AndIndented(CoalgebraExpr, AbilityIdExpr) {

  override def p[F[_]: LiftParser: Comonad]: P[OnExpr[F]] =
    (`on` *> ` ` *> Value.`value`[F] <* ` : \n+`).map { peerId =>
      OnExpr(peerId)
    }
}
