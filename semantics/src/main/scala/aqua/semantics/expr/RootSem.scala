package aqua.semantics.expr

import aqua.parser.expr.RootExpr
import aqua.raw.{ContextRaw, Raw}
import aqua.semantics.Prog
import cats.syntax.applicative.*
import cats.Monad

class RootSem[S[_]](val expr: RootExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Raw] =
    Prog.after {
      case sm: ContextRaw =>
        sm.pure[Alg]
      case m =>
        ContextRaw
          .contextPart(m)
          // TODO .getOrElse(Model.error("Root contains not a script model, it's " + m))
          .pure[Alg]

    }
}
