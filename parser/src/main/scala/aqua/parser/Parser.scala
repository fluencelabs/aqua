package aqua.parser

import cats.data.{Validated, ValidatedNec}
import aqua.parser.Ast
import aqua.parser.ParserError
import aqua.parser.LexerError
import aqua.parser.expr.RootExpr
import aqua.parser.head.HeadExpr
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import cats.{Comonad, Eval, ~>}
import cats.parse.LocationMap
import cats.parse.Parser0 as P0
import cats.Id
import aqua.parser.lift.LiftParser.LiftErrorOps


object Parser {

  import Span.spanLiftParser
  lazy val spanParser = parserSchema[Span.F]()
  import LiftParser.Implicits.idLiftParser
  lazy val idParser = parserSchema[Id]()

  def parserSchema[S[_] : LiftParser : Comonad](): P0[ValidatedNec[ParserError[S], Ast[S]]] =
    (HeadExpr.ast[S].with1 ~ RootExpr.ast[S]()).map { case (head, bodyMaybe) =>
      bodyMaybe.map(Ast(head, _))
    }

  def parser[S[_] : LiftParser : Comonad](p: P0[ValidatedNec[ParserError[S], Ast[S]]])(source: String): ValidatedNec[ParserError[S], Ast[S]] = {
    p.parseAll(source) match {
      case Right(value) => value
      case Left(e) => Validated.invalidNec(LexerError(e.wrapErr))
    }
  }

  def natParser[S[_] : LiftParser : Comonad, K[_] : Comonad]
  (p: P0[ValidatedNec[ParserError[S], Ast[S]]],
   nat: S ~> K
  )(source: String): ValidatedNec[ParserError[K], Ast[K]] = {

    parser[S](p)(source).bimap(
      e => e.map(_.mapK(nat)),
      ast => Ast[K](ast.head.map(_.mapK(nat)), ast.tree.map(_.mapK(nat)))
    )
  }
}
