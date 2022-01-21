package aqua.semantics.expr.func

import aqua.raw.ops.{AssignmentTag, FuncOp, SeqTag, XorTag}
import aqua.parser.expr.func.CatchExpr
import aqua.raw.value.ValueRaw
import aqua.raw.Raw
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class CatchSem[S[_]](val expr: CatchExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        N.beginScope(expr.name) >>
          N.define(expr.name, ValueRaw.LastError.baseType),
        (_: Boolean, g: Raw) =>
          g match {
            case FuncOp(op) =>
              N.endScope() as (XorTag
                .wrap(
                  SeqTag.wrap(
                    AssignmentTag(ValueRaw.LastError, expr.name.value).leaf,
                    op
                  )
                )
                .toFuncOp: Raw)
            case _ =>
              N.endScope() as g
          }
      )
      .abilitiesScope[S](expr.token)

}
