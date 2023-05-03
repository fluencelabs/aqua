package aqua.semantics.expr

import aqua.parser.expr.FieldTypeExpr
import aqua.raw.{Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.Monad

class FieldTypeSem[S[_]](val expr: FieldTypeExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit T: TypesAlgebra[S, Alg], D: DefinitionsAlgebra[S, Alg]): Prog[Alg, Raw] =
    T.resolveType(expr.`type`).flatMap {
      case Some(t) => D.defineDef(expr.name, t) as (TypeRaw(expr.name.value, t): Raw)
      case None => Raw.error("Field type unresolved").pure[Alg]
    }

}
