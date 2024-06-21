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
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{NamedTypeToken, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class ServiceExpr[F[_]](name: NamedTypeToken[F], id: Option[ValueToken[F]])
    extends Expr[F](ServiceExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ServiceExpr[K] =
    copy(name.mapK(fk), id.map(_.mapK(fk)))
}

object ServiceExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ArrowTypeExpr :: Nil

  override val p: Parser[ServiceExpr[Span.S]] =
    (`service` *> ` ` *> NamedTypeToken.ct ~ ValueToken.`value`.between(`(`, `)`).backtrack.?).map {
      case (name, id) =>
        ServiceExpr(name, id)
    }
}
