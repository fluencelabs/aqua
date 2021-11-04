package aqua.semantics.expr.func

import aqua.model.func.raw.{AssignmentTag, FuncOp, FuncOps, XorTag}
import aqua.model.{Model, VarModel}
import aqua.parser.expr.func.CatchExpr
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
  ): Prog[Alg, Model] =
    Prog
      .around(
        N.beginScope(expr.name) >>
          N.define(expr.name, VarModel.lastError.`type`),
        (_: Boolean, g: Model) =>
          g match {
            case op: FuncOp =>
              N.endScope() as (FuncOp.wrap(
                XorTag,
                FuncOps.seq(
                  FuncOp.leaf(AssignmentTag(VarModel.lastError, expr.name.value)),
                  op
                )
              ): Model)
            case _ =>
              N.endScope() as g
          }
      )
      .abilitiesScope[S](expr.token)

}
