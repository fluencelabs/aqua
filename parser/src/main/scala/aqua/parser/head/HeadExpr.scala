package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token.` \n+`
import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}

case class HeadExpr[F[_]]() extends HeaderExpr[F]

object HeadExpr {

  def headExprs: List[HeaderExpr.Companion] =
    ImportExpr :: Nil

  def ast[F[_]: LiftParser: Comonad]: P0[Ast.Head[F]] =
    P.repSep0(P.oneOf(headExprs.map(_.ast[F])), ` \n+`)
      .surroundedBy(` \n+`.?)
      .?
      .map {
        case Some(exprs) => Chain.fromSeq(exprs)
        case None => Chain.empty[Ast.Head[F]]
      }
      .map(exprs => Cofree(HeadExpr[F](), Eval.now(exprs)))
}
