package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class ServiceExpr[F[_]](name: Ability[F], id: Option[Value[F]]) extends Expr[F]

object ServiceExpr extends Expr.AndIndented(ArrowTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[ServiceExpr[F]] =
    (`service` *> ` ` *> Ability.ab[F] ~ Value.`value`[F].between(`(`, `)`).backtrack.?).map { case (name, id) =>
      ServiceExpr(name, id)
    }
}
