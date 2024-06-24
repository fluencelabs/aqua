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

package aqua.parser.lexer

import aqua.parser.lift.Span.S
import aqua.parser.lexer.Token.*

import cats.data.NonEmptyList
import cats.parse.Parser as P
import cats.syntax.functor.*
import cats.Comonad
import cats.arrow.FunctionK

enum NamedArg[F[_]] extends Token[F] {
  // for `name = value`
  case Full(name: Name[F], value: ValueToken[F])
  // for just `name` (short for `name = name`)
  case Short(variable: VarToken[F])

  lazy val argName: Name[F] = this match {
    case Full(name, _) => name
    case Short(variable) => variable.name
  }

  lazy val argValue: ValueToken[F] = this match {
    case Full(_, value) => value
    case Short(variable) => variable
  }

  override def as[T](v: T): F[T] = this match {
    case Full(name, value) => name.as(v)
    case Short(variable) => variable.as(v)
  }

  override def mapK[K[_]: Comonad](fk: FunctionK[F, K]): NamedArg[K] =
    this match {
      case Full(name, value) => Full(name.mapK(fk), value.mapK(fk))
      case Short(variable) => Short(variable.mapK(fk))
    }

  override def toString: String = this match {
    case Full(name, value) => s"$name = $value"
    case Short(variable) => variable.toString
  }
}

object NamedArg {

  private val namedArgFull: P[NamedArg.Full[S]] =
    P.defer(
      (Name.p.between(` *`, `/s*`) <*
        `=`.between(` *`, `/s*`)) ~
        ValueToken.`value`.between(` *`, `/s*`)
    ).map(NamedArg.Full.apply)

  private val namedArgShort: P[NamedArg.Short[S]] =
    P.defer(
      VarToken.variable.between(` *`, `/s*`)
    ).map(NamedArg.Short.apply)

  val namedArg: P[NamedArg[S]] =
    namedArgFull.backtrack | namedArgShort

  val namedArgs: P[NonEmptyList[NamedArg[S]]] =
    P.defer(` `.?.with1 ~ `(` ~ `/s*` *> comma(namedArg) <* `/s*` *> `)`)
}
