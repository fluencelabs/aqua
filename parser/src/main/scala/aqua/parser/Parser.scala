/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.parser

import aqua.parser.expr.RootExpr
import aqua.parser.head.Header
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
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
