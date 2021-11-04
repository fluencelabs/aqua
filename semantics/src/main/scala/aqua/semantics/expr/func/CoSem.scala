package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, ParTag}
import aqua.parser.expr.func.CoExpr
import aqua.semantics.Prog
import cats.syntax.applicative._
import cats.Monad

class CoSem[S[_]](val expr: CoExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp =>
        FuncOp.wrap(ParTag.Detach, g).pure[Alg]
      case g => g.pure[Alg]
    }
}
