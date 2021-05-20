package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{FuncOp, ParTag}
import aqua.parser.expr.ParExpr
import aqua.semantics.Prog
import cats.free.Free

class ParSem[F[_]](val expr: ParExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp =>
        Free.pure[Alg, Model](FuncOp.wrap(ParTag, g))
      case g => Free.pure[Alg, Model](g)
    }
}
