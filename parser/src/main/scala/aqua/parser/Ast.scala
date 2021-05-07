package aqua.parser

import aqua.parser.expr._
import aqua.parser.head.{HeadExpr, HeaderExpr, ImportExpr}
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}
import cats.{Comonad, Eval}

case class Ast[F[_]](head: Ast.Head[F], tree: Ast.Tree[F]) {

  def cata[T](folder: (Expr[F], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[F], T](tree)(folder)
}

object Ast {
  type IndentTree[F[_]] = Cofree[Chain, IndentExpr[F]]
  type Tree[F[_]] = Cofree[Chain, Expr[F]]
  type Head[F[_]] = Cofree[Chain, HeaderExpr[F]]

  def treeExprs: List[Expr.Companion] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: ConstantExpr :: FuncExpr :: Nil

  def headExprs: List[HeaderExpr.Companion] =
    ImportExpr :: Nil

  def parser[F[_]: LiftParser: Comonad](ps: Indent): P0[Ast[F]] =
    ((P.repSep0(P.oneOf(headExprs.map(_.ast[F])), ` \n+`) <* ` \n+`).? ~ P.repSep0(
      P.oneOf(treeExprs.map(_.ast[F]())),
      ` \n+`
    )).surroundedBy(` \n+`.?)
      .map {
        case (Some(head), tree) => Chain.fromSeq(head) -> Chain.fromSeq(tree)
        case (_, tree) => Chain.empty[Head[F]] -> Chain.fromSeq(tree)
      }
      .map { case (hs, ls) =>
        Ast(Cofree(HeadExpr(), Eval.now(hs)), Cofree(RootExpr(), Eval.now(ls.map(_.map(_.e)))))
      }

  def fromString[F[_]: LiftParser: Comonad](script: String): ValidatedNec[P.Error, Ast[F]] =
    Validated
      .fromEither(
        parser[F](Indent()).parseAll(script)
      )
      .leftMap(NonEmptyChain.one)
}
