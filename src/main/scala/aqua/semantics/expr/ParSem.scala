package aqua.semantics.expr

import aqua.model.{FuncOp, Model, ParModel}
import aqua.parser.expr.ParExpr
import aqua.semantics.Prog
import cats.data.NonEmptyList
import cats.free.Free

class ParSem[F[_]](val expr: ParExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Model] =
    Prog.after[Alg, Model] {
      case g: FuncOp => Free.pure[Alg, Model](ParModel(NonEmptyList.of(g)))
      case g => Free.pure[Alg, Model](g)
    }
}
