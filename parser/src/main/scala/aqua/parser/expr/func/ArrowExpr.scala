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

import aqua.parser.lexer.{ArrowTypeToken, BasicTypeToken, NamedTypeToken, TypeToken, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}
import aqua.parser.{ArrowReturnError, Ast, Expr, ParserError}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class ArrowExpr[F[_]](arrowTypeExpr: ArrowTypeToken[F])
    extends Expr[F](ArrowExpr, arrowTypeExpr) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ArrowExpr[K] =
    copy(arrowTypeExpr.mapK(fk))
}

object ArrowExpr extends Expr.AndIndented {

  val funcChildren: List[Expr.Lexem] =
    ServiceIdExpr ::
      PushToStreamExpr ::
      ForExpr ::
      Expr.defer(OnExpr) ::
      // It is important for IfExpr to be before CallArrowExpr
      // because `if (1 + 1) == 2` is parsed as if `if(1 + 1)` is an arrow call
      IfExpr ::
      ElseOtherwiseExpr ::
      TryExpr ::
      CatchExpr ::
      Expr.defer(ParSeqExpr) ::
      Expr.defer(ParExpr) ::
      Expr.defer(CoExpr) ::
      Expr.defer(JoinExpr) ::
      DeclareStreamExpr ::
      Expr.defer(ClosureExpr) ::
      AssignmentExpr ::
      // It is important for CallArrowExpr to be last
      // because it can parse prefixes of other expressions
      // e.g. `if` could be parsed as variable name
      CallArrowExpr ::
      Nil

  override val validChildren: List[Expr.Lexem] =
    ReturnExpr :: funcChildren

  override val p: Parser[ArrowExpr[Span.S]] =
    ArrowTypeToken
      .`arrowWithNames`(
        TypeToken.`typedef`
      )
      .map(ArrowExpr(_))
}
