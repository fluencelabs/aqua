package aqua.semantics.expr.func

import aqua.raw.ops.{FuncOp, XorTag}
import aqua.parser.expr.func.TryExpr
import aqua.raw.Raw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.applicative.*
import cats.Monad

class TrySem[S[_]](val expr: TryExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .after[Alg, Raw] {
        case o: FuncOp =>
          FuncOp.wrap(XorTag.LeftBiased, o).pure[Alg]
        case _ =>
          Raw.error("Wrong body of the try expression").pure[Alg]
      }
      .abilitiesScope(expr.token)
}
