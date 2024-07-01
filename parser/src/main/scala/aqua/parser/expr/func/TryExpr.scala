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
import aqua.parser.expr.func.{IfExpr, TryExpr}
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.parse.Parser as P
import cats.{Comonad, ~>}

case class TryExpr[F[_]](point: Token[F]) extends Expr[F](TryExpr, point) {

  override def mapK[K[_]: Comonad](fk: F ~> K): TryExpr[K] =
    copy(point.mapK(fk))
}

object TryExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    IfExpr.validChildren

  override val p: P[TryExpr[Span.S]] =
    `try`.lift.map(Token.lift[Span.S, Unit](_)).map(TryExpr(_))
}
