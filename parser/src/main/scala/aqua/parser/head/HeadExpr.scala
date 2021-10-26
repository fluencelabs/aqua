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
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class HeadExpr[S[_]](token: Token[S]) extends HeaderExpr[S] {

  def mapK[K[_]: Comonad](fk: S ~> K): HeadExpr[K] =
    copy(token.mapK(fk))
}

object HeadExpr {

  def headExprs: List[HeaderExpr.Companion] =
    UseFromExpr :: UseExpr :: ImportFromExpr :: ImportExpr :: ExportExpr :: Nil

  val ast: P0[Ast.Head[Span.F]] =
    (P.unit.lift0.map(Token.lift) ~ ((ModuleExpr.p <* ` \n+`).? ~
      P.repSep0(P.oneOf(headExprs.map(_.ast.backtrack)), ` \n+`).map(Chain.fromSeq))
      .surroundedBy(` \n+`.?)
      .?).map {
      case (p, Some((maybeMod, exprs))) =>
        Cofree(
          maybeMod.getOrElse(HeadExpr[Span.F](p)),
          Eval.now(exprs)
        )
      case (p, None) => Cofree(HeadExpr[Span.F](p), Eval.now(Chain.nil))
    }
}
