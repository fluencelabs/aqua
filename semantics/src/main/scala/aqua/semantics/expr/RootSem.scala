package aqua.semantics.expr

import aqua.parser.expr.RootExpr
import aqua.raw.{Raw, RawContext, RawPart}
import aqua.semantics.Prog
import cats.syntax.applicative.*
import cats.Monad

class RootSem[S[_]](val expr: RootExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Raw] =
    Prog.after {
      case sm: RawPart =>
        sm.pure[Alg]
      case m =>
        RawPart
          .contextPart(m)
          // TODO .getOrElse(Model.error("Root contains not a script model, it's " + m))
          .pure[Alg]

    }
}
