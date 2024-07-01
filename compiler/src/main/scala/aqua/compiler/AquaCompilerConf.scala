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

package aqua.compiler

import aqua.raw.ConstantRaw

/**
 * What should compiler care about during compilation – before generator backend takes its role
 *
 * @param constantsList List of known constants
 * @param relayVarName Name of the relay variable
 */
case class AquaCompilerConf(
  constants: List[ConstantRaw] = Nil,
  relayVarName: Option[String] = Some("-relay-")
)
