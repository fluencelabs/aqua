package aqua.semantics.expr

import aqua.generator.{AirGen, Gen, ParGen}
import aqua.parser.expr.ParExpr
import aqua.semantics.Prog

class ParSem[F[_]](val expr: ParExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Gen] =
    Prog.after[Alg, Gen] {
      case g: AirGen => ParGen(left = None, right = g).lift
      case g => g.lift
    }
}
