package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.raw.ops.ClosureTag
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

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after {
      case arrow: ArrowRaw =>
        // TODO: if detached, clear all locally-defined abilities
        N.defineArrow(
          expr.name,
          arrow.`type`,
          isRoot = false
        ) as ClosureTag(FuncRaw(expr.name.value, arrow), expr.detach.isDefined).funcOpLeaf

      case _ =>
        Raw.error("Closure must continue with an arrow definition").pure[Alg]
    }

}
