package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.CoalgebraExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.semantics.algebra.names.NamesAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._

class CoalgebraSem[F[_]](val expr: CoalgebraExpr[F]) extends AnyVal {

  import expr._

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
