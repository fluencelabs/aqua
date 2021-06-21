package aqua.parser.expr

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import aqua.parser.{Expr, ParserError}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser => P}
import cats.{Comonad, Eval}

case class RootExpr[F[_]](point: Token[F]) extends Expr[F](RootExpr, point)

object RootExpr extends Expr.Companion {

  def validChildren: List[Expr.Lexem] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: ConstantExpr :: FuncExpr :: Nil

  override def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Tree[F]]] =
    (P.unit.lift0.map(Token.lift[F, Unit](_)).with1 ~
      P.repSep(
        P.oneOf(RootExpr.validChildren.map(_.ast[F]())),
        ` \n+`
      ).between(` \n+`.?, `\n+ `.?)
        .map(_.foldLeft[(Chain[ParserError[F]], Chain[Tree[F]])](Chain.empty -> Chain.empty) {
          case ((errs, trees), Validated.Valid(tree)) => (errs, trees :+ tree)
          case ((errs, trees), Validated.Invalid(err)) => (errs ++ err.toChain, trees)
        })).map { case (point, (errs, trees)) =>
      NonEmptyChain
        .fromChain(errs)
        .fold[ValidatedNec[ParserError[F], Tree[F]]](
          Validated.validNec(Cofree(RootExpr[F](point), Eval.now(trees)))
        )(Validated.invalid)
    }
}
