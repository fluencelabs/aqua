package aqua.parser.ast

import aqua.{AquaError, SyntaxError}
import aqua.parser.lift.LiftParser
import cats.{Comonad, Eval}
import cats.free.Cofree
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lexer.Token._
import cats.data.{NonEmptyList, Validated, ValidatedNel}

case class Ast[F[_]](tree: Ast.Tree[F]) {}

object Ast {
  type Tree[F[_]] = Cofree[List, Expr[F]]

  def rootExprs: List[Expr.Companion] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: FuncExpr :: Nil

  def parser[F[_]: LiftParser: Comonad](ps: ParserState): P0[Ast[F]] =
    P.repSep0(
        P.oneOf(rootExprs.map(_.ast[F](ps))),
        ` \n+`
      )
      .surroundedBy(` \n+`.?)
      .map(ls => Ast(Cofree(RootExpr(), Eval.now(ls))))

  def fromString[F[_]: LiftParser: Comonad](script: String): ValidatedNel[AquaError, Ast[F]] =
    Validated
      .fromEither(
        parser[F](ParserState()).parseAll(script)
      )
      .leftMap(pe => NonEmptyList.one(SyntaxError(pe.failedAtOffset, pe.expected)))
}
