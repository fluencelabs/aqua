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
import aqua.parser.expr.func.DeclareStreamExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{BasicTypeToken, Name, Token, TypeToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.parse.Parser as P
import cats.{Comonad, ~>}

case class DeclareStreamExpr[F[_]](name: Name[F], `type`: BasicTypeToken[F])
    extends Expr[F](DeclareStreamExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): DeclareStreamExpr[K] =
    copy(name.mapK(fk), `type`.mapK(fk))
}

object DeclareStreamExpr extends Expr.Leaf {

  override val p: P[DeclareStreamExpr[Span.S]] =
    ((Name.p <* ` : `) ~ BasicTypeToken.`compositetypedef`).map { case (name, t) =>
      DeclareStreamExpr(name, t)
    }

}
