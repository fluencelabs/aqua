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

case class FuncExpr[F[_]](
  name: Name[F]
) extends Expr[F](FuncExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): FuncExpr[K] =
    copy(name.mapK(fk))
}

object FuncExpr extends Expr.Prefix(` `.?) {
  override def continueWith: List[Expr.Lexem] = ArrowExpr :: Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    (`func` *> ` ` *> Name.p[F]).map(FuncExpr(_))
}
