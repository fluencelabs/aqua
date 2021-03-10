package aqua.parser.ast

import aqua.interim.ValuesAlgebra
import aqua.interim.abilities.AbilitiesAlgebra
import aqua.interim.scope.PeerIdAlgebra
import aqua.parser.lexer.Value
import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import aqua.parser.lexer.Token._
import cats.Comonad
import cats.syntax.flatMap._

case class OnExpr[F[_]](peerId: Value[F]) extends Expr[F] {

  def program[Alg[_]](implicit
    P: PeerIdAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Unit] =
    Prog.around(
      V.ensureIsString(peerId) >> P.onPeerId(peerId) >> A.beginScope(peerId),
      (_: Unit) => A.endScope() >> P.erasePeerId()
    )

}

object OnExpr extends Expr.AndIndented(CoalgebraExpr, AbilityIdExpr) {

  override def p[F[_]: LiftParser: Comonad]: P[OnExpr[F]] =
    (`on` *> ` ` *> Value.`value`[F] <* ` : \n+`).map { peerId =>
      OnExpr(peerId)
    }
}
