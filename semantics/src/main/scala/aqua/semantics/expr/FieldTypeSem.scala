package aqua.semantics.expr

import aqua.model.{Model, TypeModel}
import aqua.parser.expr.FieldTypeExpr
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.functor._

class FieldTypeSem[F[_]](val expr: FieldTypeExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Model] =
    T.resolveType(expr.`type`).flatMap {
      case Some(t) => T.defineField(expr.name, t) as (TypeModel(expr.name.value, t): Model)
      case None => Free.pure[Alg, Model](Model.error("Field type unresolved"))
    }

}
