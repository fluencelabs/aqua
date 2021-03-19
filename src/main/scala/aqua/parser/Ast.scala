package aqua.parser

import aqua.parser.expr._
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.{AquaError, SyntaxError}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}
import cats.{Comonad, Eval}

case class Ast[F[_]](tree: Ast.Tree[F]) {

  def cata[T](folder: (Expr[F], List[T]) => Eval[T]): Eval[T] =
    Cofree.cata[List, Expr[F], T](tree)(folder)
}

object Ast {
  type Tree[F[_]] = Cofree[List, Expr[F]]

  def rootExprs: List[Expr.Companion] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: FuncExpr :: Nil

  def parser[F[_]: LiftParser: Comonad](ps: Indent): P0[Ast[F]] =
    P.repSep0(
        P.oneOf(rootExprs.map(_.ast[F](ps))),
        ` \n+`
      )
      .surroundedBy(` \n+`.?)
      .map(ls => Ast(Cofree(RootExpr(), Eval.now(ls))))

  def fromString[F[_]: LiftParser: Comonad](script: String): ValidatedNel[AquaError, Ast[F]] =
    Validated
      .fromEither(
        parser[F](Indent()).parseAll(script)
      )
      .leftMap(pe => NonEmptyList.one(SyntaxError(pe.failedAtOffset, pe.expected)))
}
