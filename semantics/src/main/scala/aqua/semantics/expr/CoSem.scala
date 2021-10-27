package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, ParTag}
import aqua.parser.expr.CoExpr
import aqua.semantics.Prog
import cats.syntax.applicative._
import cats.Monad

class CoSem[F[_]](val expr: CoExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp =>
        FuncOp.wrap(ParTag.Detach, g).pure[Alg]
      case g => g.pure[Alg]
    }
}
