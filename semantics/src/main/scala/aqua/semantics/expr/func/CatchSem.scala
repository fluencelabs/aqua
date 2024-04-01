package aqua.semantics.expr.func

import aqua.parser.expr.func.CatchExpr
import aqua.raw.Raw
import aqua.raw.ops.{AssignmentTag, FuncOp, SeqTag, TryTag}
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.names.NamesAlgebra

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class CatchSem[S[_]](val expr: CatchExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        N.define(expr.name, ValueRaw.errorType),
        (_, g: Raw) =>
          g match {
            case FuncOp(op) =>
              TryTag.Catch
                .wrap(
                  SeqTag.wrap(
                    AssignmentTag(ValueRaw.error, expr.name.value).leaf,
                    op
                  )
                )
                .toFuncOp
                .pure
            case _ =>
              Raw.error("Wrong body of the `catch` expression").pure
          }
      )
      .abilitiesScope[S](expr.token)
      .namesScope(expr.token)
}
