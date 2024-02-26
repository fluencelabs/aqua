package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
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
  ): Prog[Alg, Raw] =
    Prog.after {
      case arrow: ArrowRaw =>
        N.defineArrow(expr.name, arrow.`type`, isRoot = true) as FuncRaw(expr.name.value, arrow)

      case _ =>
        Raw.error("Func must continue with an arrow definition").pure[Alg]
    }

}
