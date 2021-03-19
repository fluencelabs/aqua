package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.AliasExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.types.TypesAlgebra
import cats.syntax.functor._

class AliasSem[F[_]](val expr: AliasExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Gen] =
    T.resolveType(expr.target).flatMap {
      case Some(t) => T.defineAlias(expr.name, t) as Gen.noop
      case None => Gen.error.lift
    }
}
