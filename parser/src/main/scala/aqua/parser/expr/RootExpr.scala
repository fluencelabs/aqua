package aqua.parser.expr

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.{LiftParser, Span}
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
  import Span.*

  def validChildren: List[Expr.Lexem] =
    ServiceExpr :: AliasExpr :: DataStructExpr :: ScopeExpr :: ConstantExpr :: FuncExpr :: Nil

  private def gatherResults[F[_]: LiftParser: Comonad](results: NonEmptyList[ValidatedNec[ParserError[F], Tree[F]]]): (Chain[ParserError[F]], Chain[Tree[F]]) = {
    results.foldLeft[(Chain[ParserError[F]], Chain[Tree[F]])](Chain.empty -> Chain.empty) {
      case ((errs, trees), Validated.Valid(tree)) => (errs, trees :+ tree)
      case ((errs, trees), Validated.Invalid(err)) => (errs ++ err.toChain, trees)
    }
  }

  private lazy val linesParser: P[NonEmptyList[ValidatedNec[ParserError[Span.S], Tree[Span.S]]]] =
    P.repSep(
      P.oneOf(RootExpr.validChildren.map(_.ast)),
      ` \n+`
    ).surroundedBy(` \n+`.?)

  private val rootToken: P0[Token[Span.S]] =
    P.unit.lift0.map(Token.lift[Span.S, Unit](_))

  private def parserSchema: P[(Token[Span.S], (Chain[ParserError[Span.S]], Chain[Tree[Span.S]]))] =
    rootToken.with1 ~
      linesParser.map(l => gatherResults(l))

  def empty: P0[ValidatedNec[ParserError[Span.S], Tree[Span.S]]] =
    (rootToken <* (Token.` \n*` *> Token.` `.? *> P.end))
      .map(point => Validated.validNec(Cofree(RootExpr[Span.S](point), Eval.now(Chain.empty))))

  // Could handle empty body
  def ast0: P0[ValidatedNec[ParserError[Span.S], Tree[Span.S]]] =
    // `empty` is first to handle errors from `ast` at a first place
    empty.backtrack | ast

  override val ast: P[ValidatedNec[ParserError[Span.S], Tree[Span.S]]] =
    parserSchema
        .map { case (point, (errs, trees)) =>
      NonEmptyChain
        .fromChain(errs)
        .fold[ValidatedNec[ParserError[Span.S], Tree[Span.S]]](
          Validated.validNec(Cofree(RootExpr[Span.S](point), Eval.now(trees)))
        )(Validated.invalid)
    }
}
