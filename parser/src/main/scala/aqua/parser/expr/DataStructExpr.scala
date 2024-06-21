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

package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.NamedTypeToken
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class DataStructExpr[F[_]](name: NamedTypeToken[F]) extends Expr[F](DataStructExpr, name) {
  override def mapK[K[_]: Comonad](fk: F ~> K): DataStructExpr[K] = copy(name.mapK(fk))
}

object DataStructExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = FieldTypeExpr :: Nil

  override val p: Parser[DataStructExpr[Span.S]] =
    `data` *> ` ` *> NamedTypeToken.ct.map(DataStructExpr(_))
}
