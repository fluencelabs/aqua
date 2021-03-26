package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._

case class ForExpr[F[_]](item: Name[F], iterable: Value[F]) extends Expr[F]

object ForExpr extends Expr.AndIndented(Expr.defer(OnExpr), ParExpr, CallArrowExpr, AbilityIdExpr) {

  override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] =
    ((`for` *> ` ` *> Name.p[F] <* ` <- `) ~ Value.`value`[F]).map { case (item, iterable) =>
      ForExpr(item, iterable)
    }
}
