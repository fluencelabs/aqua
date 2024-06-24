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

package aqua.backend.api

import aqua.backend.air.AirBackend
import aqua.backend.{Backend, Generated}
import aqua.res.AquaRes
import aqua.definitions.{LabeledProductTypeDef, ArrowTypeDef, ServiceDef}

object APIBackend extends Backend {

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val airGenerated = AirBackend.generate(res)

      val services = res.services.map { srv =>
          val functions = LabeledProductTypeDef(
            srv.members.map { case (n, a) => (n, ArrowTypeDef(a)) }
          )

          ServiceDef(srv.defaultId.map(s => s.replace("\"", "")), functions, srv.name)
      }.toList

      Generated("", "", airGenerated.flatMap(_.air).toList, services) :: Nil
    }
}
