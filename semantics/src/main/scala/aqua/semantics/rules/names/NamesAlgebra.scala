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

package aqua.semantics.rules.names

import aqua.parser.lexer.{LiteralToken, Name, Token, ValueToken}
import aqua.types.{ArrowType, StreamType, Type}

import cats.InjectK

trait NamesAlgebra[S[_], Alg[_]] {

  def read(name: Name[S], mustBeDefined: Boolean = true): Alg[Option[Type]]

  // TODO can be implemented via read?
  def constantDefined(name: Name[S]): Alg[Option[Type]]

  def readArrow(name: Name[S]): Alg[Option[ArrowType]]

  def define(name: Name[S], `type`: Type): Alg[Boolean]

  def defineInternal(name: String, `type`: Type): Alg[Boolean]

  def derive(name: Name[S], `type`: Type, derivedFrom: Set[String]): Alg[Boolean]

  def getDerivedFrom(fromNames: List[Set[String]]): Alg[List[Set[String]]]

  def defineConstant(name: Name[S], `type`: Type): Alg[Boolean]

  def defineArrow(name: Name[S], gen: ArrowType, isRoot: Boolean): Alg[Boolean]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]
}
