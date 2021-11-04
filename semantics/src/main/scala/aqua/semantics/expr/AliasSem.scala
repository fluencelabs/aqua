package aqua.semantics.expr

import aqua.model.{Model, TypeModel}
import aqua.parser.expr.AliasExpr
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor._
import cats.Monad
import cats.Applicative
import cats.syntax.flatMap._

class AliasSem[S[_]](val expr: AliasExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit T: TypesAlgebra[S, Alg]): Prog[Alg, Model] =
    T.resolveType(expr.target).flatMap {
      case Some(t) => T.defineAlias(expr.name, t) as (TypeModel(expr.name.value, t): Model)
      case None => Applicative[Alg].pure(Model.error("Alias type unresolved"))
    }
}
