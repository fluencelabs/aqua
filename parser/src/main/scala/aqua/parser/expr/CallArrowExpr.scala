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
  args: List[Value[F]],
  parPrefix: Option[F[Unit]]
) extends Expr[F] with ParPrefix[F]

object CallArrowExpr extends Expr.Companion {

  override def p[F[_]: LiftParser: Comonad]: P[CallArrowExpr[F]] =
    (ParPrefix.p.with1 ~ ((Name.p[F] <* ` <- `).backtrack.?.with1 ~
      ((Ability.ab[F] <* `.`).?.with1 ~
        Name.p[F] ~
        comma0(Value.`value`[F]).between(`(`, `)`)))).map {
      case (parPrefix, (variable, ((ability, funcName), args))) =>
        CallArrowExpr(variable, ability, funcName, args, parPrefix)
    }

}
