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

package aqua.helpers.data

import aqua.errors.Errors.internalError

import cats.data.NonEmptyList
import cats.syntax.option.*

/**
 * Short for PathName. Represents name with parts separated by `.`
 */
final case class PName(
  parts: NonEmptyList[String]
) {

  lazy val simple: Option[String] =
    Option.when(parts.length == 1)(parts.head)

  lazy val isSimple: Boolean = simple.isDefined

  lazy val value: String = parts.toList.mkString(".")

  lazy val splits: List[(PName, PName)] = {
    val partsList = parts.toList
    (1 until parts.length).toList.map(i =>
      PName(NonEmptyList.fromListUnsafe(partsList.take(i))) ->
        PName(NonEmptyList.fromListUnsafe(partsList.drop(i)))
    )
  }

  override def toString(): String = value
}

object PName {

  def simpleUnsafe(name: String): PName =
    if (name.isEmpty || name.contains("."))
      internalError(s"Invalid PName: $name")
    else PName(NonEmptyList.one(name))
}
