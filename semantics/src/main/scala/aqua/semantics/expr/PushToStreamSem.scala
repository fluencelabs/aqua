package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, PushToStreamTag}
import aqua.parser.expr.PushToStreamExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import cats.free.Free
import cats.syntax.functor._

class PushToStreamSem[F[_]](val expr: PushToStreamExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        N.define(expr.variable, vm.lastType) as (FuncOp
          .leaf(PushToStreamTag(vm, expr.variable.value)): Model)
      case _ => Free.pure[Alg, Model](Model.error("Cannot resolve stream type"))
    }

}
