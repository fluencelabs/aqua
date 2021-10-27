package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, XorTag}
import aqua.parser.expr.ElseOtherwiseExpr
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.syntax.applicative._
import cats.Monad

class ElseOtherwiseSem[F[_]](val expr: ElseOtherwiseExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit A: AbilitiesAlgebra[F, Alg]): Prog[Alg, Model] =
    Prog
      .after[Alg, Model] {
        case g: FuncOp => FuncOp.wrap(XorTag, g).pure[Alg]
        case g => g.pure[Alg]
      }
      .abilitiesScope(expr.token)
}
