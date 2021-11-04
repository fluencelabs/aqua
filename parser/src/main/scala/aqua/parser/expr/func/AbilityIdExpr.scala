package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.AbilityIdExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.parse.Parser as P
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class AbilityIdExpr[F[_]](ability: Ability[F], id: Value[F])
    extends Expr[F](AbilityIdExpr, ability) {
  
  def mapK[K[_]: Comonad](fk: F ~> K): AbilityIdExpr[K] =
    copy(ability.copy(fk(ability.name)), id.mapK(fk))
  
}

object AbilityIdExpr extends Expr.Leaf {

  override val p: P[AbilityIdExpr[Span.S]] =
    ((Ability.dotted <* ` `) ~ Value.`value`).map { case (ability, id) =>
      AbilityIdExpr(ability, id)
    }

}
