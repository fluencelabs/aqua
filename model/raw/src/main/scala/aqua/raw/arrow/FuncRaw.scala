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

package aqua.raw.arrow

import aqua.raw.RawPart
import aqua.types.Type

case class FuncRaw(
  name: String,
  arrow: ArrowRaw
) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  def addAbilityName(s: String): RawPart = copy(arrow = arrow.copy(`type` = Type.addAbilityNameArrow(s, arrow.`type`)))

  override def rawPartType: Type = arrow.`type`

  // vars that we capture from external space (outer functions, etc)
  lazy val capturedVars: Set[String] = {
    val freeBodyVars: Set[String] = arrow.body.usesVarNames.value
    val argsNames = arrow.`type`.domain
      .toLabelledList()
      .map { case (name, _) => name }
      .toSet

    freeBodyVars -- argsNames
  }
}
