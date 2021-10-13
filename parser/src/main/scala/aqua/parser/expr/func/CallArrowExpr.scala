package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}
import cats.{Comonad, ~>}

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

  def ability[F[_]: LiftParser: Comonad]: P0[Option[Ability[F]]] = (Ability.dotted[F] <* `.`).?
  def functionCallWithArgs[F[_]: LiftParser: Comonad] = Name.p[F]
    ~ comma0(Value.`value`[F].surroundedBy(`/s*`)).between(`(` <* `/s*`, `/s*` *> `)`)
  def funcCall[F[_]: LiftParser: Comonad] = ability.with1 ~ functionCallWithArgs
  
  def funcOnly[F[_]: LiftParser: Comonad] = funcCall.map {
    case (ab, (name, args)) =>
      CallArrowExpr(Nil, ab, name, args)
  }

  override def p[F[_]: LiftParser: Comonad]: P[CallArrowExpr[F]] = {
    val variables: P0[Option[NonEmptyList[Name[F]]]] = (comma(Name.p[F]) <* ` <- `).backtrack.?

    (variables.with1 ~ funcCall.withContext("Only results of a function call can be written to a stream")
      ).map {
      case (variables, (ability, (funcName, args))) =>
        CallArrowExpr(variables.toList.flatMap(_.toList), ability, funcName, args)
    }
  }

}
