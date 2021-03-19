package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import cats.syntax.functor._

class ReturnSem[F[_]](val expr: ReturnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit V: ValuesAlgebra[F, Alg]): Prog[Alg, Gen] =
    V.resolveType(expr.value).as(Gen.noop)
}
