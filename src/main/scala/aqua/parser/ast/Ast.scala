package aqua.parser.ast

import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lexer.Token._

case class Ast[F[_]](tree: Ast.Tree[F]) {}

object Ast {
  type Tree[F[_]] = Cofree[List, Expr[F]]

  def rootExprs: List[Expr.Companion] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: DefFuncExpr :: Nil

  def parser[F[_]: LiftParser: Comonad](ps: ParserState = ParserState()): P0[Ast[F]] =
    P.repSep0(
        P.oneOf(rootExprs.map(_.ast[F](ps))),
        ` \n+`
      )
      .surroundedBy(` \n+`.?)
      .map(ls => Ast(Cofree(RootExpr(), Eval.now(ls))))
}
