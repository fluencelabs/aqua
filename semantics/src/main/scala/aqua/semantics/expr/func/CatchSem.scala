package aqua.semantics.expr

import aqua.model.func.raw.{AssignmentTag, FuncOp, FuncOps, XorTag}
import aqua.model.{Model, VarModel}
import aqua.parser.expr.CatchExpr
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.parser.expr.func.CatchExpr
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.Monad

class CatchSem[F[_]](val expr: CatchExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
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
      .abilitiesScope[F](expr.token)

}
