package aqua.parser.ast

import aqua.interim.ValuesAlgebra
import aqua.interim.abilities.AbilitiesAlgebra
import aqua.interim.names.NamesAlgebra
import aqua.interim.types.TypesAlgebra
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import cats.Comonad
import cats.free.Free
import cats.syntax.flatMap._

case class CoalgebraExpr[F[_]](
  variable: Option[Name[F]],
  ability: Option[Ability[F]],
  funcName: Name[F],
  args: List[Value[F]]
) extends Expr[F] {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Unit] =
    ability
      .fold(N.readArrow(funcName))(A.getArrow(_, funcName))
      .flatMap(at =>
        V.checkArguments(at, args) >> variable
          .fold(Free.pure[Alg, Unit](()))(exportVar =>
            at.res.fold(
              // TODO: error! we're trying to export variable, but function has no export type
              Free.pure[Alg, Unit](())
            )(resType => N.define(exportVar, resType))
          )
      )

}

object CoalgebraExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: P[CoalgebraExpr[F]] =
    ((Name.p[F] <* `<-`).backtrack.?.with1 ~
      ((Ability.ab[F] <* `.`).?.with1 ~
        Name.p[F] ~
        comma0(Value.`value`[F]).between(`(`, `)`))).map {
      case (variable, ((ability, funcName), args)) =>
        CoalgebraExpr(variable, ability, funcName, args)
    }

}
