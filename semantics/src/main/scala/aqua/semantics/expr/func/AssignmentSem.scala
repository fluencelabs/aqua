package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{AssignmentTag, FuncOp}
import aqua.parser.expr.func.AssignmentExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import cats.free.Free
import cats.syntax.functor.*

class AssignmentSem[F[_]](val expr: AssignmentExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        N.define(expr.variable, vm.lastType) as (FuncOp
          .leaf(AssignmentTag(vm, expr.variable.value)): Model)
      case _ => Free.pure[Alg, Model](Model.error("Cannot resolve assignment type"))
    }

}
