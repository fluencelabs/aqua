package aqua.parser

import aqua.parser.Expr.{ParserError, ResultError}
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
  type Tree[F[_]] = Cofree[Chain, Expr[F]]
  type Head[F[_]] = Cofree[Chain, HeaderExpr[F]]

  def treeExprs: List[Expr.RootCompanion] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: ConstantExpr :: FuncExpr :: Nil

  def headExprs: List[HeaderExpr.Companion] =
    ImportExpr :: Nil

  def parser[F[_]: LiftParser: Comonad](): P0[Either[NonEmptyChain[ResultError[F]], Ast[F]]] =
    ((P.repSep0(P.oneOf(headExprs.map(_.ast[F])), ` \n+`) <* ` \n+`).? ~ P.repSep0(
      P.oneOf(treeExprs.map(_.ast[F]())),
      ` \n+`
    )).surroundedBy(` \n+`.?)
      .map {
        case (Some(head), tree) => Chain.fromSeq(head) -> Chain.fromSeq(tree)
        case (_, tree) => Chain.empty[Head[F]] -> Chain.fromSeq(tree)
      }
      .map { case (hs, lss) =>
        val errs = lss.collect { case Left(e) =>
          e
        }

        NonEmptyChain
          .fromChain(errs)
          .fold[Either[NonEmptyChain[ResultError[F]], Ast[F]]] {
            val valid = lss.collect { case Right(v) =>
              v
            }
            Right(Ast(Cofree(HeadExpr(), Eval.now(hs)), Cofree(RootExpr(), Eval.now(valid))))
          }(e => Left(e))
      }

  def fromString[F[_]: LiftParser: Comonad](script: String): ValidatedNec[ResultError[F], Ast[F]] =
    Validated
      .fromEither(
        parser[F]().parseAll(script) match {
          case Left(e) => Left(NonEmptyChain.one(ParserError[F](e)))
          case Right(r) => r
        }
      )
}
