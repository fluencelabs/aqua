package aqua.semantics.expr

import aqua.generator.{Gen, ScriptGen}
import aqua.parser.expr.RootExpr
import aqua.semantics.Prog
import cats.syntax.semigroup._

import scala.collection.immutable.Queue

class RootSem[F[_]](val expr: RootExpr[F]) extends AnyVal {

  def program[Alg[_]]: Prog[Alg, Gen] =
    Prog.after(a => (a |+| ScriptGen(Queue.empty)).lift)
}
