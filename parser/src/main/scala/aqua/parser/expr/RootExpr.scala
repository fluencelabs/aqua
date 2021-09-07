package aqua.parser.expr

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.{Expr, ParserError}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser0 as P0, Parser as P}
import cats.{Comonad, Eval}
import cats.~>

case class RootExpr[F[_]](point: Token[F]) extends Expr[F](RootExpr, point) {

  override def mapK[K[_]: Comonad](fk: F ~> K): RootExpr[K] =
    copy(point.mapK(fk))
}

object RootExpr extends Expr.Companion {

  def validChildren: List[Expr.Lexem] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: ConstantExpr :: FuncExpr :: Nil

  private def gatherResults[F[_]: LiftParser: Comonad](results: NonEmptyList[ValidatedNec[ParserError[F], Tree[F]]]): (Chain[ParserError[F]], Chain[Tree[F]]) = {
    results.foldLeft[(Chain[ParserError[F]], Chain[Tree[F]])](Chain.empty -> Chain.empty) {
      case ((errs, trees), Validated.Valid(tree)) => (errs, trees :+ tree)
      case ((errs, trees), Validated.Invalid(err)) => (errs ++ err.toChain, trees)
    }
  }

  private def linesParser[F[_]: LiftParser: Comonad](): P[NonEmptyList[ValidatedNec[ParserError[F], Tree[F]]]] =
    P.repSep(
      P.oneOf(RootExpr.validChildren.map(_.ast[F]())),
      ` \n+`
    ).surroundedBy(` \n+`.?)

  private def rootToken[F[_]: LiftParser: Comonad]: P0[Token[F]] =
    P.unit.lift0.map(Token.lift[F, Unit](_))

  private def parserSchema[F[_]: LiftParser: Comonad](): P[(Token[F], (Chain[ParserError[F]], Chain[Tree[F]]))] =
    rootToken.with1 ~
      linesParser().map(l => gatherResults(l))

  def empty[F[_] : LiftParser : Comonad](): P0[ValidatedNec[ParserError[F], Tree[F]]] =
    (rootToken <* (Token.` \n*` *> Token.` `.? *> P.end))
      .map(point => Validated.validNec(Cofree(RootExpr[F](point), Eval.now(Chain.empty))))

  // Could handle empty body
  def ast0[F[_]: LiftParser: Comonad](): P0[ValidatedNec[ParserError[F], Tree[F]]] =
    // `empty` is first to handle errors from `ast` at a first place
    empty().backtrack | ast()

  override def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Tree[F]]] =
    parserSchema()
        .map { case (point, (errs, trees)) =>
      NonEmptyChain
        .fromChain(errs)
        .fold[ValidatedNec[ParserError[F], Tree[F]]](
          Validated.validNec(Cofree(RootExpr[F](point), Eval.now(trees)))
        )(Validated.invalid)
    }
}
