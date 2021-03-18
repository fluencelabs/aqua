package aqua.ast.expr

import aqua.ast.gen.{Gen, ScriptGen}
import aqua.ast.{Expr, Prog}
import cats.syntax.semigroup._

import scala.collection.immutable.Queue

case class RootExpr[F[_]]() extends Expr[F] {

  def program[Alg[_]]: Prog[Alg, Gen] =
    Prog.after(a => (a |+| ScriptGen(Queue.empty)).lift)
}

object RootExpr
