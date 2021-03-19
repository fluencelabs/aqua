package aqua.semantics.expr

import aqua.model.{Model, ScriptModel}
import aqua.parser.expr.RootExpr
import aqua.semantics.Prog
import cats.free.Free

class RootSem[F[_]](val expr: RootExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after {
      case sm: ScriptModel => Free.pure[Alg, Model](sm)
      case _ => Free.pure[Alg, Model](Model.error)
    }
}
