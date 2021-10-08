package aqua.semantics.expr.func

import aqua.model.{Model, ReturnModel}
import aqua.parser.expr.func.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import cats.syntax.functor.*
import cats.syntax.traverse.*

class ReturnSem[F[_]](val expr: ReturnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit V: ValuesAlgebra[F, Alg]): Prog[Alg, Model] =
    expr.values.traverse(V.resolveType) as {
      println(s"VALUES: ${expr.values}")
      (ReturnModel: Model)
    }
}
