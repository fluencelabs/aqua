package aqua.semantics.expr

import aqua.model.{FuncOp, Model, OnModel}
import aqua.parser.expr.OnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.scope.PeerIdAlgebra
import cats.syntax.flatMap._
import cats.syntax.functor._

class OnSem[F[_]](val expr: OnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    P: PeerIdAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      V.ensureIsString(expr.peerId) >> P.onPeerId(expr.peerId) >> A.beginScope(expr.peerId),
      (_: Unit, ops: Model) =>
        A.endScope() >> P.erasePeerId() as (ops match {
          case op: FuncOp =>
            OnModel(ValuesAlgebra.valueToData(expr.peerId), op)
          case _ => Model.error
        })
    )
}
