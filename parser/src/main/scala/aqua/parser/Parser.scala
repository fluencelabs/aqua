package aqua.parser

import aqua.parser.Ast.Tree
import aqua.parser.{Ast, LexerError, ParserError}
import aqua.parser.expr.RootExpr
import aqua.parser.head.HeadExpr
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser.LiftErrorOps
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import cats.data.{Validated, ValidatedNec}
import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.{Comonad, Eval, Id, ~>}


object Parser extends scribe.Logging {

  import Span.spanLiftParser
  val spanParser = parserSchema[Span.F]()
  import LiftParser.Implicits.idLiftParser
  lazy val idParser = parserSchema[Id]()

  def parserSchema[S[_] : LiftParser : Comonad](): P0[ValidatedNec[ParserError[S], Ast[S]]] = {
    logger.trace("creating schema...")
    val parser = (HeadExpr.ast[S] ~ RootExpr.ast0[S]()).map { case (head, bodyMaybe) =>
      bodyMaybe.map(Ast(head, _))
    }
    logger.trace("schema created")
    parser
  }

  def parser[S[_] : LiftParser : Comonad](p: P0[ValidatedNec[ParserError[S], Ast[S]]])(source: String): ValidatedNec[ParserError[S], Ast[S]] = {
    p.parseAll(source) match {
      case Right(value) => value
      case Left(e) => Validated.invalidNec(LexerError(e.wrapErr))
    }
  }

  def natParser[S[_] : LiftParser : Comonad, K[_] : Comonad](
    p: P0[ValidatedNec[ParserError[S], Ast[S]]],
    nat: S ~> K
  )(source: String): ValidatedNec[ParserError[K], Ast[K]] =
    parser[S](p)(source).bimap(
      e => e.map(_.mapK(nat)),
      ast => Ast[K](ast.head.map(_.mapK(nat)), ast.tree.map(_.mapK(nat)))
    )
}
