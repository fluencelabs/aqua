package aqua.semantics.expr

import aqua.model.{Model, ReturnModel}
import aqua.parser.expr.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.applicative._
import cats.Monad

class ReturnSem[F[_]](val expr: ReturnExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit V: ValuesAlgebra[F, Alg]): Prog[Alg, Model] =
    expr.values.traverse(V.resolveType) as (ReturnModel: Model)
}
