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
import aqua.parser.lift.Span
import cats.parse.Parser
import cats.{Comonad, ~>}

case class AbilityExpr[F[_]](name: NamedTypeToken[F]) extends Expr[F](AbilityExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): AbilityExpr[K] =
    copy(name.mapK(fk))
}

object AbilityExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = FieldTypeExpr :: ArrowTypeExpr :: Nil

  override val p: Parser[AbilityExpr[Span.S]] =
    (`ability` *> ` ` *> NamedTypeToken.ct).map(AbilityExpr(_))
}
