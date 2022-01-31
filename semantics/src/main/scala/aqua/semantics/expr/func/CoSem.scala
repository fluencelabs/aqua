package aqua.semantics.expr.func

import aqua.raw.ops.{FuncOp, ParTag}
import aqua.parser.expr.func.CoExpr
import aqua.raw.Raw
import aqua.semantics.Prog
import cats.syntax.applicative.*
import cats.Monad

class CoSem[S[_]](val expr: CoExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Raw] =
    Prog.after[Alg, Raw] {
      case FuncOp(g) =>
        ParTag.Detach.wrap(g).toFuncOp.pure[Alg]
      case g => g.pure[Alg]
    }
}
