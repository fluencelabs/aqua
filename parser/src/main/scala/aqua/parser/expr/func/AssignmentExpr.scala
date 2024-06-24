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
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{CollectionToken, Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.parse.Parser as P
import cats.{Comonad, ~>}

case class AssignmentExpr[F[_]](
  variable: Name[F],
  value: ValueToken[F]
) extends Expr[F](AssignmentExpr, variable) {
  def mapK[K[_]: Comonad](fk: F ~> K): AssignmentExpr[K] = copy(variable.mapK(fk), value.mapK(fk))
}

object AssignmentExpr extends Expr.Leaf {

  override val p: P[AssignmentExpr[Span.S]] =
    ((Name.variable <* ` = `).with1 ~ ValueToken.`value`).flatMap { case (variable, value) =>
      value match {
        case CollectionToken(_, values) =>
          if (values.isEmpty)
            P.failWith(
              "Assigning empty array to a variable is prohibited. You can create an array with values (like '[a, b, c]') or use '[]' in place."
            )
          else P.pure(AssignmentExpr(variable, value))
        case _ =>
          P.pure(AssignmentExpr(variable, value))
      }
    }
}
