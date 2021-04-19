package aqua.semantics.expr

import aqua.model.{Model, TypeModel}
import aqua.parser.expr.AliasExpr
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.functor._

class AliasSem[F[_]](val expr: AliasExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Model] =
    T.resolveType(expr.target).flatMap {
      case Some(t) => T.defineAlias(expr.name, t) as (TypeModel(expr.name.value, t): Model)
      case None => Free.pure[Alg, Model](Model.error("Alias type unresolved"))
    }
}
