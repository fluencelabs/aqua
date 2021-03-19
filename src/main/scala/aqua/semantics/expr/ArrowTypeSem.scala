package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.ArrowTypeExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
import cats.syntax.functor._

class ArrowTypeSem[F[_]](val expr: ArrowTypeExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg], A: AbilitiesAlgebra[F, Alg]): Prog[Alg, Gen] =
    T.resolveArrowDef(expr.`type`).flatMap {
      case Some(t) => A.defineArrow(expr.name, t) as Gen.noop
      case None => Gen.error.lift
    }

}
