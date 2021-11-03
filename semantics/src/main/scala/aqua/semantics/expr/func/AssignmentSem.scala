package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{AssignmentTag, FuncOp}
import aqua.parser.expr.func.AssignmentExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class AssignmentSem[S[_]](val expr: AssignmentExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        N.define(expr.variable, vm.lastType) as (FuncOp
          .leaf(AssignmentTag(vm, expr.variable.value)): Model)
      case _ => Model.error("Cannot resolve assignment type").pure[Alg]
    }

}
