package aqua.parser.expr

import aqua.parser.lexer.Token.{`par`, _}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser0

@deprecated("Make it ParExpr again", "20.05.2021")
trait ParPrefix[F[_]] {
  def parPrefix: Option[F[Unit]]
}

object ParPrefix {
  def p[F[_]: LiftParser: Comonad]: Parser0[Option[F[Unit]]] = (`par`.lift <* ` `).backtrack.?
}
