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

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{LiteralToken, Token, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.either.*
import cats.~>

case class ExportExpr[F[_]](pubs: NonEmptyList[FromExpr.NameOrAbAs[F]]) extends HeaderExpr[F] {

  override def token: Token[F] =
    pubs.head.bimap(_._1, _._1).fold(identity, identity)

  override def mapK[K[_]: Comonad](fk: F ~> K): ExportExpr[K] =
    copy(FromExpr.mapK(pubs)(fk))
}

object ExportExpr extends HeaderExpr.Companion {

  override val p: Parser[ExportExpr[Span.S]] =
    (`_export` *> ` `) *> comma(FromExpr.nameOrAbAs).map(ExportExpr(_))
}
