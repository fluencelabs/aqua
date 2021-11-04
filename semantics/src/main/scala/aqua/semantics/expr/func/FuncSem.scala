package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.{ArrowModel, FuncModel}
import aqua.parser.expr.FuncExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import cats.{Applicative, Monad}
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class FuncSem[S[_]](val expr: FuncExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    Prog.after {
      case arrow: ArrowModel =>
        N.defineArrow(expr.name, arrow.`type`, isRoot = true) as FuncModel(expr.name.value, arrow)

      case m =>
        Model.error("Func must continue with an arrow definition").pure[Alg]
    }

}
