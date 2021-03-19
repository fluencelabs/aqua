package aqua.semantics.expr

import aqua.model.Model
import aqua.parser.expr.AbilityIdExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.syntax.flatMap._
import cats.syntax.functor._

class AbilityIdSem[F[_]](val expr: AbilityIdExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit A: AbilitiesAlgebra[F, Alg], V: ValuesAlgebra[F, Alg]): Prog[Alg, Model] =
    V.ensureIsString(expr.id) >> A.setServiceId(expr.ability, expr.id) as Model.empty
}
