package aqua.semantics.expr.func

import aqua.raw.ops.{FuncOp, TryTag}
import aqua.parser.expr.func.TryExpr
import aqua.raw.Raw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.Monad

class TrySem[S[_]](val expr: TryExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      // Without type of ops specified
      // scala compiler fails to compile this
      .after((ops: Raw) =>
        ops match {
          case FuncOp(op) =>
            for {
              restricted <- FuncOpSem.restrictStreamsInScope(op)
              tag = TryTag.wrap(restricted)
            } yield tag.toFuncOp
          case _ =>
            Raw.error("Wrong body of the `try` expression").pure[Alg]
        }
      )
      .abilitiesScope(expr.token)
      .namesScope(expr.token)
}
