package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token.` \n+`
import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}

case class HeadExpr[F[_]](module: Option[ModuleExpr[F]]) extends HeaderExpr[F]

object HeadExpr {

  def headExprs: List[HeaderExpr.Companion] =
    UseExpr :: ImportExpr :: Nil

  def ast[F[_]: LiftParser: Comonad]: P0[Ast.Head[F]] =
    ((ModuleExpr.p[F] <* ` \n+`).? ~
      P.repSep0(P.oneOf(headExprs.map(_.ast[F])), ` \n+`).map(Chain.fromSeq))
      .surroundedBy(` \n+`.?)
      .?
      .map {
        case Some((maybeMod, exprs)) =>
          Cofree(
            HeadExpr[F](maybeMod),
            Eval.now(exprs)
          )
        case None => Cofree(HeadExpr[F](None), Eval.now(Chain.nil))
      }
}
