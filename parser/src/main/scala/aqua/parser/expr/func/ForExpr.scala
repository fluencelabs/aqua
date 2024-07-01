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
import aqua.parser.expr.func.ForExpr.NameOrPair
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.syntax.either.*
import cats.{Comonad, ~>}

case class ForExpr[F[_]](
  item: NameOrPair[F],
  iterable: ValueToken[F],
  mode: Option[ForExpr.Mode]
) extends Expr[F](ForExpr, iterable) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ForExpr[K] =
    copy(item.bimap(p => (p._1.mapK(fk), p._2.mapK(fk)), v => v.mapK(fk)), iterable.mapK(fk))
}

object ForExpr extends Expr.AndIndented {
  enum Mode { case ParMode, TryMode, RecMode }

  type NameOrPair[S[_]] = Either[(Name[S], Name[S]), Name[S]]

  private val pair: P[(Name[S], Name[S])] =
    (Name.p <* (` `.? ~ `,` ~ ` `.?)) ~ Name.p

  val nameOrPair: P[NameOrPair[S]] = pair.backtrack.eitherOr(Name.p).map(_.swap)

  override def validChildren: List[Expr.Lexem] = ArrowExpr.funcChildren

  private lazy val modeP: P[Mode] =
    (` ` *> (
      `par`.as(Mode.ParMode) |
        `try`.as(Mode.TryMode) |
        `rec`.as(Mode.RecMode)
    ).lift).map(_.extract)

  override def p: P[ForExpr[Span.S]] =
    ((`for` *> ` ` *> nameOrPair <* ` <- `) ~ ValueToken.`value` ~ modeP.?).map {
      case ((item, iterable), mode) =>
        ForExpr(item, iterable, mode)
    }
}
