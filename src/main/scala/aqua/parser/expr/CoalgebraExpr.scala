package aqua.parser.expr

import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.semantics.algebra.names.NamesAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
import aqua.generator.{Gen, ServiceCallGen}
import aqua.parser.Expr
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
          V.checkArguments(at.`type`, args) >> variable
            .fold(Free.pure[Alg, Boolean](true))(exportVar =>
              at.`type`.res.fold(
                // TODO: error! we're trying to export variable, but function has no export type
                Free.pure[Alg, Boolean](false)
              )(resType => N.define(exportVar, resType))
            ) >> at.gen[F, Alg](args, variable).widen[Gen]
        case None =>
          Gen.error.lift[Alg]
      }

}

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
