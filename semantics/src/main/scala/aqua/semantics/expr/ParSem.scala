package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, ParTag}
import aqua.parser.expr.ParExpr
import aqua.semantics.Prog
import cats.free.Free
import cats.syntax.applicative._
import cats.Monad

class ParSem[F[_]](val expr: ParExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp =>
        FuncOp.wrap(ParTag, g).pure[Alg]
      case g => g.pure[Alg]
    }
}
