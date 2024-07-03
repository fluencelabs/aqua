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

/**
 * Short for SimpleName. Represents name without `.`
 */
final case class SName private (
  name: String
) {

  lazy val toPName: PName =
    PName.fromSName(this)
}

object SName {

  def nameUnsafe(name: String): SName =
    if (name.isEmpty || name.contains("."))
      internalError(s"Invalid SName: $name")
    else SName(name)
}
