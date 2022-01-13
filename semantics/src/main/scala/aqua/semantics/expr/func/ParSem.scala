package aqua.semantics.expr.func

import aqua.raw.ops.{FuncOp, ParTag}
import aqua.parser.expr.func.ParExpr
import aqua.raw.Raw
import aqua.semantics.Prog
import cats.Monad
import cats.syntax.applicative.*

class ParSem[S[_]](val expr: ParExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Raw] =
    Prog.after[Alg, Raw] {
      case g: FuncOp =>
        FuncOp.wrap(ParTag, g).pure[Alg]
      case g => g.pure[Alg]
    }
}
