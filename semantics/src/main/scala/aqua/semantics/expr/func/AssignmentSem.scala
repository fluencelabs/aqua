package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.raw.{AssignmentTag, FuncOp}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.parser.expr.func.AssignmentExpr
import aqua.semantics.rules.names.NamesAlgebra
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.Monad

class AssignmentSem[F[_]](val expr: AssignmentExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        N.define(expr.variable, vm.lastType) as (FuncOp
          .leaf(AssignmentTag(vm, expr.variable.value)): Model)
      case _ => Model.error("Cannot resolve assignment type").pure[Alg]
    }

}
