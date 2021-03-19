package aqua.semantics.expr

import aqua.generator.{AirGen, Gen}
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
  ): Prog[Alg, Gen] =
    Prog.around(
      V.ensureIsString(expr.peerId) >> P.onPeerId(expr.peerId) >> A.beginScope(expr.peerId),
      (_: Unit, ops: Gen) =>
        A.endScope() >> P.erasePeerId() as (ops match {
          case air: AirGen =>
            air.wrap(c => (c.copy(peerId = ValuesAlgebra.valueToData(expr.peerId)), _.copy(peerId = c.peerId)))
          case _ => ops
        })
    )
}
