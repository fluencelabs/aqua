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

package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{PropertyToken, ValueToken}
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.{Comonad, ~>}

case class JoinExpr[F[_]](values: NonEmptyList[ValueToken[F]])
    extends Expr[F](JoinExpr, values.head) {

  override def mapK[K[_]: Comonad](fk: F ~> K): JoinExpr[K] =
    copy(values.map(_.mapK(fk)))
}

object JoinExpr extends Expr.Leaf {

  override val p: Parser[JoinExpr[Span.S]] =
    (`join` *> ` ` *> comma(PropertyToken.property)).map(JoinExpr(_))
}
