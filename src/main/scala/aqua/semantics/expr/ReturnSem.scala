package aqua.semantics.expr

import aqua.model.Model
import aqua.parser.expr.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import cats.syntax.functor._

class ReturnSem[F[_]](val expr: ReturnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit V: ValuesAlgebra[F, Alg]): Prog[Alg, Model] =
    V.resolveType(expr.value) as Model.empty
}
