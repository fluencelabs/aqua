package aqua.parser.expr

import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.Name
import aqua.parser.lift.LiftParser
import aqua.parser.{Ast, Expr}
import cats.Comonad
import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

trait Visibility
case object Default extends Visibility
case object Pub extends Visibility
case object Export extends Visibility

case class FuncExpr[F[_]](
  name: Name[F],
  visibility: Visibility
) extends Expr[F](FuncExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): FuncExpr[K] =
    copy(name.mapK(fk))
}

object FuncExpr extends Expr.Prefix(` `.?) {
  override def continueWith: List[Expr.Lexem] = ArrowExpr :: Nil

  override val p: Parser[FuncExpr[Span.S]] =
    (((`pub`.map(_ => Pub) | `export`.map(_ => Export)).?.with1 <* ` `) ~ (`func` *> ` ` *> Name.p)).map {
      case (visibility, name) => FuncExpr(name, visibility.getOrElse(Default))
    }
}
