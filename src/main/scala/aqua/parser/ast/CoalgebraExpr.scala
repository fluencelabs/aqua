package aqua.parser.ast

import aqua.parser.lexer.{Ability, ArrowName, Value, Var}
import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import cats.Comonad

case class CoalgebraExpr[F[_]](
  variable: Option[Var[F]],
  ability: Option[Ability[F]],
  funcName: ArrowName[F],
  args: List[Value[F]]
) extends Expr[F]

object CoalgebraExpr extends Expr.Companion {

  override def p[F[_]: LiftParser: Comonad]: P[CoalgebraExpr[F]] =
    ((Var.p[F] <* `<-`).backtrack.?.with1 ~
      ((Ability.ab[F] <* `.`).?.with1 ~
        ArrowName.an[F] ~
        comma0(Value.`value`[F]).between(`(`, `)`))).map {
      case (variable, ((ability, funcName), args)) =>
        CoalgebraExpr(variable, ability, funcName, args)
    }

}
