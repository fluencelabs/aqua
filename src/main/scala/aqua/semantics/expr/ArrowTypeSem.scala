package aqua.semantics.expr

import aqua.model.Model
import aqua.parser.expr.ArrowTypeExpr
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.functor._

class ArrowTypeSem[F[_]](val expr: ArrowTypeExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg], A: AbilitiesAlgebra[F, Alg]): Prog[Alg, Model] =
    T.resolveArrowDef(expr.`type`).flatMap {
      case Some(t) => A.defineArrow(expr.name, t) as Model.empty("Arrow type generates no model")
      case None => Free.pure[Alg, Model](Model.error("Arrow type unresolved"))
    }

}
