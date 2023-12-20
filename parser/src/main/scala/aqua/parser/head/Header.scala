package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.` \n+`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}
import cats.{Comonad, Eval}
import cats.~>

object Header {

  def headExprs: List[HeaderExpr.Companion] =
    ModuleExpr :: UseFromExpr :: UseExpr :: ImportFromExpr :: ImportExpr :: ExportExpr :: Nil

  val p: P0[Ast.Head[Span.S]] =
    P.repSep0(
      P.oneOf(headExprs.map(_.p.backtrack)),
      ` \n+`
    ).surroundedBy(` \n+`.?)
      .map(Chain.fromSeq)
}
