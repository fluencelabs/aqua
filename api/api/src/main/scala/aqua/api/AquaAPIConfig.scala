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

package aqua.api

import aqua.compiler.AquaCompilerConf
import aqua.model.transform.TransformConfig

enum TargetType:
  case TypeScriptType, JavaScriptType, AirType

case class AquaAPIConfig(
  targetType: TargetType = TargetType.AirType,
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false,
  tracing: Boolean = false,
  noEmptyResponse: Boolean = true
) {

  def getTransformConfig: TransformConfig = {
    val config = TransformConfig(
      tracing = Option.when(tracing)(TransformConfig.TracingConfig.default),
      noEmptyResponse = noEmptyResponse,
      noXor = noXor
    )

    if (noRelay) config.copy(relayVarName = None)
    else config
  }

  def getCompilerConfig: AquaCompilerConf = {
    val config = AquaCompilerConf()

    if (noRelay) config.copy(relayVarName = None)
    else config
  }
}
