package aqua.semantics.expr

import aqua.parser.expr.AliasExpr
import aqua.raw.{ErroredPart, Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.Applicative
import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class AliasSem[S[_]](val expr: AliasExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using T: TypesAlgebra[S, Alg]): Prog[Alg, Raw] =
    T.resolveType(expr.target).flatMap {
      case Some(t) => T.defineAlias(expr.name, t) as (TypeRaw(expr.name.value, t): Raw)
      case None => Applicative[Alg].pure(ErroredPart(expr.name.value))
    }
}
