package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, ParTag}
import aqua.parser.expr.func.ParExpr
import aqua.semantics.Prog
import cats.Monad
import cats.syntax.applicative.*

class ParSem[S[_]](val expr: ParExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp =>
        FuncOp.wrap(ParTag, g).pure[Alg]
      case g => g.pure[Alg]
    }
}
