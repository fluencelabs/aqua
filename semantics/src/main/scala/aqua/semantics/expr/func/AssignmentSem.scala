package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.types.ArrowType
import aqua.raw.value.CallArrowRaw
import aqua.raw.ops.{AssignmentTag, ClosureTag, FuncOp}
import aqua.parser.expr.func.AssignmentExpr
import aqua.raw.arrow.FuncRaw
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
  ): Prog[Alg, Raw] =
    Prog.after { a =>
      V.valueToRaw(expr.value).flatMap {
        case Some(vm) =>
//          println("expr in sem: " + expr)
//          println("vm: " + vm)
//          println("vm type: " + vm.`type`)
          vm.`type` match {
            case at @ ArrowType(_, _) =>
              N.defineArrow(expr.variable, at, false) as (AssignmentTag(
                vm,
                expr.variable.value
              ).funcOpLeaf: Raw)
            case _ =>
              N.derive(expr.variable, vm.`type`, vm.varNames) as (AssignmentTag(
                vm,
                expr.variable.value
              ).funcOpLeaf: Raw)
          }

        case _ => Raw.error("Cannot resolve assignment type").pure[Alg]
      }
    }

}
