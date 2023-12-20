package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token.` \n+`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser as P, Parser0 as P0}
import aqua.parser.lexer.Token
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan, S}
import aqua.parser.Ast.Head

case class HeadExpr[S[_]](token: Token[S]) extends HeaderExpr[S] {

  def mapK[K[_]: Comonad](fk: S ~> K): HeadExpr[K] =
    copy(token.mapK(fk))
}

object HeadExpr {

  def headExprs: List[HeaderExpr.Companion] =
    UseFromExpr :: UseExpr :: ImportFromExpr :: ImportExpr :: ExportExpr :: Nil

  val headers: P0[Chain[Head[S]]] = P.repSep0(P.oneOf(headExprs.map(_.ast.backtrack)), ` \n+`).map(Chain.fromSeq)
  
  val ast: P0[Ast.Head[Span.S]] =
    ((ModuleExpr.p <* ` \n+`).? ~ headers)
      .surroundedBy(` \n+`.?)
      .?.flatMap {
      case Some((Some(mod), exprs)) =>
        P.pure(Cofree(
          mod,
          Eval.now(exprs)
        ))
      case _ => P.failWith("Aqua file must start with 'aqua AquaName' string")
    }
}
