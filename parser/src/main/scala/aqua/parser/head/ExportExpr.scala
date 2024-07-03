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

package aqua.parser.head

import aqua.parser.lexer.QName
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{LiteralToken, Token, ValueToken}
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>

case class ExportExpr[F[_]](
  token: Token[F],
  pubs: NonEmptyList[QName.As[F]]
) extends HeaderExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ExportExpr[K] =
    copy(token = token.mapK(fk), pubs = pubs.map(_.mapK(fk)))
}

object ExportExpr extends HeaderExpr.Companion {

  override val p: Parser[ExportExpr[Span.S]] =
    ((`export` *> ` `) *> comma(QName.as)).lift.map(point =>
      ExportExpr(
        Token.lift(point.void),
        point.extract
      )
    )
}
