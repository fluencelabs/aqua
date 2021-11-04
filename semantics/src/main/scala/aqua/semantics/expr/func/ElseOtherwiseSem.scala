package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, XorTag}
import aqua.parser.expr.func.ElseOtherwiseExpr
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.syntax.applicative._
import cats.Monad

class ElseOtherwiseSem[S[_]](val expr: ElseOtherwiseExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit A: AbilitiesAlgebra[S, Alg]): Prog[Alg, Model] =
    Prog
      .after[Alg, Model] {
        case g: FuncOp => FuncOp.wrap(XorTag, g).pure[Alg]
        case g => g.pure[Alg]
      }
      .abilitiesScope(expr.token)
}
