package aqua.semantics.expr

import aqua.model.func.FuncModel
import aqua.model.{Model, ScriptModel, ServiceModel, TypeModel}
import aqua.parser.expr.RootExpr
import aqua.semantics.Prog
import cats.data.Chain
import cats.free.Free

class RootSem[F[_]](val expr: RootExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after {
      case sm: ScriptModel => Free.pure[Alg, Model](sm)
      case m =>
        Free.pure[Alg, Model](
          ScriptModel
            .toScriptPart(m)
            .getOrElse(Model.error("Root contains not a script model, it's " + m))
        )
    }
}
