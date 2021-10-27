package aqua.semantics.expr

import aqua.model.{Model, ScriptModel}
import aqua.parser.expr.RootExpr
import aqua.semantics.Prog
import cats.syntax.applicative._
import cats.Monad

class RootSem[F[_]](val expr: RootExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Model] =
    Prog.after {
      case sm: ScriptModel =>
        sm.pure[Alg]
      case m =>
        ScriptModel
          .toScriptPart(m)
          .getOrElse(Model.error("Root contains not a script model, it's " + m))
          .pure[Alg]

    }
}
