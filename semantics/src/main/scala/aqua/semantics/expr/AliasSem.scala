package aqua.semantics.expr

import aqua.parser.expr.AliasExpr
import aqua.raw.{Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor.*
import cats.Monad
import cats.Applicative
import cats.syntax.flatMap.*

class AliasSem[S[_]](val expr: AliasExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit T: TypesAlgebra[S, Alg]): Prog[Alg, Raw] =
    T.resolveType(expr.target).flatMap {
      case Some(t) => T.defineAlias(expr.name, t) as (TypeRaw(expr.name.value, t): Raw)
      case None => Applicative[Alg].pure(Raw.error("Alias type unresolved"))
    }
}
