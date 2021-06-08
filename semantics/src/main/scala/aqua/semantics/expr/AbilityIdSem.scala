package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{AbilityIdTag, FuncOp}
import aqua.parser.expr.AbilityIdExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._

class AbilityIdSem[F[_]](val expr: AbilityIdExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    A: AbilitiesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.ensureIsString(expr.id) >> V.valueToModel(
      expr.id
    ) >>= {
      case Some(id) =>
        A.setServiceId(expr.ability, expr.id, id) as (FuncOp.leaf(
          AbilityIdTag(id, expr.ability.value)
        ): Model)
      case _ => Free.pure[Alg, Model](Model.error("Cannot resolve ability ID"))
    }
}
