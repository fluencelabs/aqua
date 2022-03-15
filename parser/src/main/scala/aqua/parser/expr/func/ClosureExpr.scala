package aqua.parser.expr.func

import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Name
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.{Ast, Expr, ParserError}
import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ClosureExpr[F[_]](
  name: Name[F],
  detach: Option[F[Unit]]
) extends Expr[F](ClosureExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ClosureExpr[K] =
    copy(name.mapK(fk), detach.map(fk.apply))
}

object ClosureExpr extends Expr.Prefix() {
  override def continueWith: List[Expr.Lexem] = Expr.defer(ArrowExpr) :: Nil

  override val p: Parser[ClosureExpr[Span.S]] =
    ((Name.p <* ` ` <* `=`) ~ (` ` *> `func`.lift).backtrack.?).map(ClosureExpr(_, _))

}
