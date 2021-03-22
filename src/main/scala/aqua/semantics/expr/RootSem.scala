package aqua.semantics.expr

import aqua.model.{FuncModel, Model, ScriptModel}
import aqua.parser.expr.RootExpr
import aqua.semantics.Prog
import cats.free.Free

import scala.collection.immutable.Queue

class RootSem[F[_]](val expr: RootExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after {
      case sm: ScriptModel => Free.pure[Alg, Model](sm)
      case fm: FuncModel => Free.pure[Alg, Model](ScriptModel(Queue(fm)))
      case m => Free.pure[Alg, Model](Model.error("Root contains not a script model, it's " + m))
    }
}
