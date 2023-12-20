package aqua.parser

import aqua.parser.expr.RootExpr
import aqua.parser.head.HeadExpr
import aqua.parser.lift.LiftParser.LiftErrorOps
import aqua.parser.lift.Span.S
import aqua.parser.lift.{LiftParser, Span}
import cats.data.{Validated, ValidatedNec}
import cats.parse.{Parser as P, Parser0 as P0}
import cats.{~>, Comonad}
import cats.free.Cofree

object Parser extends scribe.Logging {
  lazy val spanParser: P0[ValidatedNec[ParserError[S], Ast[S]]] = parserSchema

  def parserSchema: P0[ValidatedNec[ParserError[Span.S], Ast[Span.S]]] = {
    logger.trace("creating schema...")
    val parser = (HeadExpr.ast ~ RootExpr.ast0).map { case (head, bodyMaybe) =>
      bodyMaybe.map(Ast(head, _))
    }
    logger.trace("schema created")
    parser
  }

  def parse[S[_]: LiftParser: Comonad](
    p: P0[ValidatedNec[ParserError[S], Ast[S]]]
  )(source: String): ValidatedNec[ParserError[S], Ast[S]] = {
    p.parseAll(source) match {
      case Right(value) => value
      case Left(e) => Validated.invalidNec(LexerError(e.wrapErr))
    }
  }

  def natParser[S[_]: LiftParser: Comonad, K[_]: Comonad](
    p: P0[ValidatedNec[ParserError[S], Ast[S]]],
    nat: S ~> K
  )(source: String): ValidatedNec[ParserError[K], Ast[K]] =
    parse[S](p)(source).bimap(
      e => e.map(_.mapK(nat)),
      ast => Ast[K](ast.head.map(_.mapK(nat)), ast.tree.map(_.mapK(nat)))
    )
}
