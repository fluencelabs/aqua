package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}

case class CoalgebraExpr[F[_]](
  variable: Option[Name[F]],
  ability: Option[Ability[F]],
  funcName: Name[F],
  args: List[Value[F]]
) extends Expr[F]

object CoalgebraExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[CoalgebraExpr[F]] =
    ((Name.p[F] <* ` <- `).backtrack.?.with1 ~
      ((Ability.ab[F] <* `.`).?.with1 ~
        Name.p[F] ~
        comma0(Value.`value`[F]).between(`(`, `)`))).map {
      case (variable, ((ability, funcName), args)) =>
        CoalgebraExpr(variable, ability, funcName, args)
    }

}
