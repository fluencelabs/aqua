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

package aqua.backend.ts

import aqua.backend.{Backend, Generated, OutputFile}
import aqua.res.AquaRes
import cats.data.NonEmptyChain

case class TypeScriptBackend(isOldFluenceJs: Boolean = false, client: String = Backend.client) extends Backend {

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(TypeScriptTypes(client), isJs = false, isOldFluenceJs)
      Generated(TypeScriptBackend.ext, script, airs) :: Nil
    }
}

object TypeScriptBackend {
  val ext = ".ts"
}
