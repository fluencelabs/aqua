package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.raw.ops.{ClosureTag, FuncOp}
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import cats.Applicative
import cats.data.Chain
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.Monad

class ClosureSem[S[_]](val expr: ClosureExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after {
      case arrow: ArrowRaw =>
        N.defineArrow(
          expr.name,
          arrow.`type`,
          isRoot = false
        ) as FuncOp.leaf(ClosureTag(FuncRaw(expr.name.value, arrow)))

      case m =>
        Raw.error("Closure must continue with an arrow definition").pure[Alg]
    }

}
