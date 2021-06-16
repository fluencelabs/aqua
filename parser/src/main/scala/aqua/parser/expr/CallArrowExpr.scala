package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}

case class CallArrowExpr[F[_]](
  variable: Option[Name[F]],
  ability: Option[Ability[F]],
  funcName: Name[F],
  args: List[Value[F]]
) extends Expr[F](CallArrowExpr, funcName)

object CallArrowExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[CallArrowExpr[F]] =
    ((Name.p[F] <* ` <- `).backtrack.?.with1 ~
      ((Ability.ab[F] <* `.`).?.with1 ~
        Name.p[F] ~
        comma0(`s*`.with1 *> Value.`value`[F]).between(`(`, `)`))).map {
      case (variable, ((ability, funcName), args)) =>
        CallArrowExpr(variable, ability, funcName, args)
    }

}
