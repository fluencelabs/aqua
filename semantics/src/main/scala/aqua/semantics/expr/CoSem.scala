package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, ParTag}
import aqua.parser.expr.CoExpr
import aqua.semantics.Prog
import cats.free.Free

class CoSem[F[_]](val expr: CoExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp =>
        Free.pure[Alg, Model](FuncOp.wrap(ParTag.Detach, g))
      case g => Free.pure[Alg, Model](g)
    }
}
