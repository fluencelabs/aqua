package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ServiceExpr[F[_]](name: Ability[F], id: Option[Value[F]])
    extends Expr[F](ServiceExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ServiceExpr[K] =
    copy(name.mapK(fk), id.map(_.mapK(fk)))
}

object ServiceExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ArrowTypeExpr :: Nil

  override val p: Parser[ServiceExpr[Span.S]] =
    (`service` *> ` ` *> Ability.ab ~ Value.`value`.between(`(`, `)`).backtrack.?).map {
      case (name, id) =>
        ServiceExpr(name, id)
    }
}
