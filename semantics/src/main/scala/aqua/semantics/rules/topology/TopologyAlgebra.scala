package aqua.semantics.rules.topology

import aqua.parser.expr.func.OnExpr
import aqua.parser.lexer.Token

trait TopologyAlgebra[S[_], Alg[_]] {

  def beginScope(token: OnExpr[S]): Alg[Unit]

  def endScope(): Alg[Unit]
}
