package aqua.semantics.expr.func

import aqua.model.Model
import aqua.model.func.raw.{FuncOp, XorTag}
import aqua.parser.expr.func.TryExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.applicative._
import cats.Monad

class TrySem[S[_]](val expr: TryExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    Prog
      .after[Alg, Model] {
        case o: FuncOp =>
          FuncOp.wrap(XorTag.LeftBiased, o).pure[Alg]
        case _ =>
          Model.error("Wrong body of the try expression").pure[Alg]
      }
      .abilitiesScope(expr.token)
}
