package aqua.semantics.expr

import aqua.model.{Model, ReturnModel}
import aqua.parser.expr.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import cats.syntax.functor._
import cats.syntax.traverse._

class ReturnSem[F[_]](val expr: ReturnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit V: ValuesAlgebra[F, Alg]): Prog[Alg, Model] =
    expr.values.traverse(V.resolveType) as (ReturnModel: Model)
}
