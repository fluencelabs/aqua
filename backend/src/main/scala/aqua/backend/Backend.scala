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

package aqua.backend

import aqua.model.AquaContext
import aqua.res.AquaRes

/**
 * Compiler backend generates output based on the processed model
 */
trait Backend {

  /**
   * Generate the result based on the given [[AquaRes]]
   *
   * @param aqua Source file context, processed, transformed
   * @return Zero or more [[Generated]] objects, based on arguments
   */
  def generate(aqua: AquaRes): Seq[Generated]
}

object Backend {

  val client = "IFluenceClient$$"

  trait Transform extends Backend {
    def transform(ex: AquaContext): AquaRes
  }
}
