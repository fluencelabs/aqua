package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}

case class AbilityIdExpr[F[_]](ability: Ability[F], id: Value[F])
    extends Expr[F](AbilityIdExpr, ability)

object AbilityIdExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[AbilityIdExpr[F]] =
    ((Ability.dotted[F] <* ` `) ~ Value.`value`).map { case (ability, id) =>
      AbilityIdExpr(ability, id)
    }

}
