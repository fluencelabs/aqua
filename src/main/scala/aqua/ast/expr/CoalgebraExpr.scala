package aqua.ast.expr

import aqua.ast.{Expr, Gen, Prog}
import aqua.ast.algebra.ValuesAlgebra
import aqua.ast.algebra.abilities.AbilitiesAlgebra
import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.free.Free
import cats.parse.{Parser => P}
import cats.syntax.flatMap._
import cats.syntax.functor._

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
  ): Prog[Alg, Gen] =
    ability
      .fold(N.readArrow(funcName))(A.getArrow(_, funcName))
      .flatMap {
        case Some(at) =>
          V.checkArguments(at, args) >> variable
            .fold(Free.pure[Alg, Boolean](true))(exportVar =>
              at.res.fold(
                // TODO: error! we're trying to export variable, but function has no export type
                Free.pure[Alg, Boolean](false)
              )(resType => N.define(exportVar, resType))
            // TODO: if it's a service, get service id, etc
            ) as Gen("Coalgebra expression")
        case None =>
          Free.pure(Gen("Coalgebra expression errored"))
      }

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
