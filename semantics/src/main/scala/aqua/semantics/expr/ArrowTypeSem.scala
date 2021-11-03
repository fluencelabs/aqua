package aqua.semantics.expr

import aqua.model.{Model, TypeModel}
import aqua.parser.expr.ArrowTypeExpr
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.Monad

class ArrowTypeSem[S[_]](val expr: ArrowTypeExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    T.resolveArrowDef(expr.`type`).flatMap {
      case Some(t) => A.defineArrow(expr.name, t) as (TypeModel(expr.name.value, t): Model)
      case None => Model.error("Arrow type unresolved").pure[Alg]
    }

}
