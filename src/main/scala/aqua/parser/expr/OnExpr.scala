package aqua.parser.expr

import aqua.semantics.Prog
import aqua.semantics.algebra.ValuesAlgebra
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.semantics.algebra.scope.PeerIdAlgebra
import aqua.generator.{AirGen, ArrowGen, Gen}
import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.flatMap._
import cats.syntax.functor._

case class OnExpr[F[_]](peerId: Value[F]) extends Expr[F] {

  def program[Alg[_]](implicit
    P: PeerIdAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Gen] =
    Prog.around(
      V.ensureIsString(peerId) >> P.onPeerId(peerId) >> A.beginScope(peerId),
      (_: Unit, ops: Gen) =>
        A.endScope() >> P.erasePeerId() as (ops match {
          case air: AirGen =>
            air.wrap(c => (c.copy(peerId = ArrowGen.valueToData(peerId)), _.copy(peerId = c.peerId)))
          case _ => ops
        })
    )

}

object OnExpr extends Expr.AndIndented(CoalgebraExpr, AbilityIdExpr) {

  override def p[F[_]: LiftParser: Comonad]: P[OnExpr[F]] =
    (`on` *> ` ` *> Value.`value`[F] <* ` : \n+`).map { peerId =>
      OnExpr(peerId)
    }
}
