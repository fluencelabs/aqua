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

import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Name
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}
import aqua.parser.{Ast, Expr, ParserError}

import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser
import cats.{Comonad, ~>}

case class ClosureExpr[F[_]](
  name: Name[F],
  detach: Option[F[Unit]]
) extends Expr[F](ClosureExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ClosureExpr[K] =
    copy(name.mapK(fk), detach.map(fk.apply))
}

object ClosureExpr extends Expr.Prefix(` `.?) {
  override def continueWith: List[Expr.Lexem] = Expr.defer(ArrowExpr) :: Nil

  override val p: Parser[ClosureExpr[Span.S]] =
    ((Name.p <* ` ` <* `=`) ~ (` ` *> `func`.lift).backtrack.?).map(ClosureExpr(_, _))

}
