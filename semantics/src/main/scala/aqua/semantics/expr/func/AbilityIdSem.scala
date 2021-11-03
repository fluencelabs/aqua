package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{AbilityIdTag, FuncOp}
import aqua.parser.expr.func.AbilityIdExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class AbilityIdSem[S[_]](val expr: AbilityIdExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    V.ensureIsString(expr.id) >> V.valueToModel(
      expr.id
    ) >>= {
      case Some(id) =>
        A.setServiceId(expr.ability, expr.id, id) as (FuncOp.leaf(
          AbilityIdTag(id, expr.ability.value)
        ): Model)
      case _ => Model.error("Cannot resolve ability ID").pure[Alg]
    }
}
