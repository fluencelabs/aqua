package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token.` \n+`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lexer.Token

case class HeadExpr[S[_]](token: Token[S]) extends HeaderExpr[S]

object HeadExpr {

  def headExprs: List[HeaderExpr.Companion] =
    UseFromExpr :: UseExpr :: ImportFromExpr :: ImportExpr :: ExportExpr :: Nil

  def ast[S[_]: LiftParser: Comonad]: P0[Ast.Head[S]] =
    (P.unit.lift0.map(Token.lift) ~ ((ModuleExpr.p[S] <* ` \n+`).? ~
      P.repSep0(P.oneOf(headExprs.map(_.ast[S].backtrack)), ` \n+`).map(Chain.fromSeq))
      .surroundedBy(` \n+`.?)
      .?).map {
      case (p, Some((maybeMod, exprs))) =>
        Cofree(
          maybeMod.getOrElse(HeadExpr[S](p)),
          Eval.now(exprs)
        )
      case (p, None) => Cofree(HeadExpr[S](p), Eval.now(Chain.nil))
    }
}
