package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{FuncOp, OnTag}
import aqua.parser.expr.OnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import cats.Traverse
import cats.data.Chain
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.flatMap._

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
        A.endScope() >> (ops match {
          case op: FuncOp =>
            (
              V.valueToModel(expr.peerId),
              Traverse[List].traverse(expr.via)(V.valueToModel).map(_.flatten)
            ).mapN {
              case (Some(om), via) =>
                FuncOp.wrap(
                  OnTag(
                    om,
                    Chain.fromSeq(via)
                  ),
                  op
                )
              case _ =>
                Model.error("OnSem: Impossible error")
            }

          case m => Free.pure[Alg, Model](Model.error("On body is not an op, it's " + m))
        })
    )
}
