package aqua.parser.ast

import aqua.parser.lexer.{Ability, Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class ServiceExpr[F[_]](name: Ability[F], id: Option[Value[F]]) extends Expr[F] {}

object ServiceExpr extends Expr.Companion {

  override def wrapsExprs: List[Expr.Companion] = ArrowTypeExpr :: Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[ServiceExpr[F]] =
    (`service` *> ` ` *> Ability.ab[F] ~ Value.`value`[F].between(`(`, `)`).backtrack.? <* ` : \n+`).map {
      case (name, id) => ServiceExpr(name, id)
    }
}
