package aqua.parser

import aqua.parser.expr.RootExpr
import aqua.parser.head.Header
import aqua.parser.lift.LiftParser.LiftErrorOps
import aqua.parser.lift.Span.S
import aqua.parser.lift.{LiftParser, Span}

import cats.data.{Validated, ValidatedNec}
import cats.parse.{Parser as P, Parser0 as P0}
import cats.syntax.validated.*
import cats.{Comonad, ~>}

object Parser extends scribe.Logging {
  lazy val spanParser: P0[ValidatedNec[ParserError[S], Ast[S]]] = parserSchema

  def parserSchema: P0[ValidatedNec[ParserError[Span.S], Ast[Span.S]]] = {
    logger.trace("creating schema...")
    val parser = (Header.p ~ RootExpr.ast0).map { case (head, bodyMaybe) =>
      bodyMaybe.map(Ast(head, _))
    }
    logger.trace("schema created")
    parser
  }

  def parse[S[_]: LiftParser: Comonad](
    p: P0[ValidatedNec[ParserError[S], Ast[S]]]
  )(source: String): ValidatedNec[ParserError[S], Ast[S]] =
    p.parseAll(source).left.map(e => LexerError(e.wrapErr).invalidNec).merge

  def natParser[S[_]: LiftParser: Comonad, K[_]: Comonad](
    p: P0[ValidatedNec[ParserError[S], Ast[S]]],
    nat: S ~> K
  )(source: String): ValidatedNec[ParserError[K], Ast[K]] =
    parse(p)(source).bimap(
      e => e.map(_.mapK(nat)),
      ast => ast.mapK(nat)
    )
}
