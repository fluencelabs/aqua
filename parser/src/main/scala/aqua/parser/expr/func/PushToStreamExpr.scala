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
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.expr.func.PushToStreamExpr.ValueOrPair
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, Token, ValueToken}
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.parse.Parser as P
import cats.syntax.either.*
import cats.{Comonad, ~>}

case class PushToStreamExpr[F[_]](
  stream: Name[F],
  value: ValueOrPair[F]
) extends Expr[F](PushToStreamExpr, stream) {

  override def mapK[K[_]: Comonad](fk: F ~> K): PushToStreamExpr[K] =
    copy(stream.mapK(fk), value.bimap(p => (p._1.mapK(fk), p._2.mapK(fk)), v => v.mapK(fk)))
}

object PushToStreamExpr extends Expr.Leaf {

  type ValueOrPair[S[_]] = Either[(ValueToken[S], ValueToken[S]), ValueToken[S]]

  private val valueOrPair: P[ValueOrPair[S]] =
    P.repSep(ValueToken.`value` <* ` `.?, 1, 2, `,`).map { l =>
      l.tail match {
        case r :: _ => Left(l.head -> r)
        case _ => Right(l.head)
      }
    }

  override val p: P[PushToStreamExpr[Span.S]] =
    ((Name.p <* ` <<- `).with1 ~ valueOrPair).map { case (variable, value) =>
      PushToStreamExpr(variable, value)
    }
}
