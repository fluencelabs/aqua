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

import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Name
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}
import aqua.parser.{Ast, Expr}

import cats.Comonad
import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser
import cats.~>

case class FuncExpr[F[_]](
  name: Name[F]
) extends Expr[F](FuncExpr, name) {

  override def mapK[K[_]: Comonad](fk: F ~> K): FuncExpr[K] =
    copy(name.mapK(fk))
}

object FuncExpr extends Expr.Prefix(` `.?) {
  override def continueWith: List[Expr.Lexem] = ArrowExpr :: Nil

  override val p: Parser[FuncExpr[Span.S]] =
    (`func` *> ` ` *> Name.p).map(FuncExpr(_))
}
