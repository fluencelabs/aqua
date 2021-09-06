package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import cats.~>

case class CallArrowExpr[F[_]](
  variables: List[Name[F]],
  ability: Option[Ability[F]],
  funcName: Name[F],
  args: List[Value[F]]
) extends Expr[F](CallArrowExpr, funcName) {

  def mapK[K[_]: Comonad](fk: F ~> K): CallArrowExpr[K] =
    copy(
      variables.map(_.mapK(fk)),
      ability.map(_.mapK(fk)),
      funcName.mapK(fk),
      args.map(_.mapK(fk))
    )
}

object CallArrowExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[CallArrowExpr[F]] =
    ((comma(Name.p[F]) <* ` <- `).backtrack.?.with1 ~
      ((Ability.dotted[F] <* `.`).?.with1 ~
        Name.p[F] ~
        comma0(Value.`value`[F].surroundedBy(`/s*`)).between(`(` <* `/s*`, `/s*` *> `)`))).map {
      case (variables, ((ability, funcName), args)) =>
        CallArrowExpr(variables.toList.flatMap(_.toList), ability, funcName, args)
    }

}
