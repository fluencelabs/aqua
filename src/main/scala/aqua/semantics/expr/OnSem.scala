package aqua.semantics.expr

import aqua.model.{FuncOp, Model, OnTag}
import aqua.parser.expr.OnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.data.Chain
import cats.syntax.flatMap._
import cats.syntax.functor._

class OnSem[F[_]](val expr: OnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      expr.via.foldLeft(
        V.ensureIsString(expr.peerId)
      ) { case (acc, v) =>
        acc >> V.ensureIsString(v)
      }
        >> A.beginScope(expr.peerId),
      (_: Unit, ops: Model) =>
        A.endScope() as (ops match {
          case op: FuncOp =>
            // the way to the node may be lost if there will be chains of `on` without calls
            // so peerId is added to the path
            val path = expr.via :+ expr.peerId
            val returnPath = path.reverse
            val returnLeaf = {
              FuncOp.leaf(
                OnTag(
                  ValuesAlgebra.valueToModel(returnPath.last),
                  returnPath.map(ValuesAlgebra.valueToModel)
                )
              )
            }

            FuncOp.node(
              OnTag(
                ValuesAlgebra.valueToModel(expr.peerId),
                path.map(ValuesAlgebra.valueToModel)
              ),
              Chain(
                op,
                returnLeaf
              )
            )

          case m => Model.error("On body is not an op, it's " + m)
        })
    )
}
