package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{FuncOp, XorTag}
import aqua.parser.expr.ElseOtherwiseExpr
import aqua.semantics.Prog
import cats.free.Free

class ElseOtherwiseSem[F[_]](val expr: ElseOtherwiseExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp => Free.pure[Alg, Model](FuncOp.wrap(XorTag, g))
      case g => Free.pure[Alg, Model](g)
    }
}
