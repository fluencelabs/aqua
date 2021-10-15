package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{ClosureTag, FuncOp}
import aqua.model.func.{ArrowModel, FuncModel}
import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import cats.Applicative
import cats.data.Chain
import cats.free.Free
import cats.syntax.functor.*

class ClosureSem[F[_]](val expr: ClosureExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.after {
      case arrow: ArrowModel =>
        N.defineArrow(
          expr.name,
          arrow.`type`,
          isRoot = false
        ) as FuncOp.leaf(ClosureTag(FuncModel(expr.name.value, arrow)))

      case m =>
        Free.pure[Alg, Model](Model.error("Closure must continue with an arrow definition"))
    }

}
