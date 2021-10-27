package aqua.semantics.expr.func

import aqua.model.func.{ArrowModel, FuncModel}
import aqua.model.Model
import aqua.parser.expr.FuncExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import cats.Applicative
import cats.data.Chain
import cats.free.Free
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class FuncSem[F[_]](val expr: FuncExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.after {
      case arrow: ArrowModel =>
        N.defineArrow(expr.name, arrow.`type`, isRoot = true) as FuncModel(expr.name.value, arrow)

      case m =>
        Free.pure[Alg, Model](Model.error("Func must continue with an arrow definition"))
    }

}
