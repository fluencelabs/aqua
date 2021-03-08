package aqua.parser.ast

import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class ServiceExpr[F[_]](name: Ability[F], id: Option[Value[F]]) extends Expr[F] {}

object ServiceExpr extends Expr.AndIndented(ArrowTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[ServiceExpr[F]] =
    (`service` *> ` ` *> Ability.ab[F] ~ Value.`value`[F].between(`(`, `)`).backtrack.? <* ` : \n+`).map {
      case (name, id) => ServiceExpr(name, id)
    }
}
