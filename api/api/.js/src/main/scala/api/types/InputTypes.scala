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

package api.types

import aqua.api.AquaAPIConfig
import aqua.api.TargetType.*
import aqua.js.{FunctionDefJs, ServiceDefJs}

import cats.data.Validated.{invalidNec, validNec}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}
import scala.scalajs.js.|

@JSExportTopLevel("Input")
@JSExportAll
class Input(
  val input: String
)

@JSExportTopLevel("Path")
@JSExportAll
class Path(
  val path: String
)

@JSExportTopLevel("Call")
@JSExportAll
class Call(
  val functionCall: String,
  val arguments: js.Dynamic,
  val input: Path | Input
)

@JSExportTopLevel("AquaConfig")
@JSExportAll
class AquaConfig(
  val logLevel: js.UndefOr[String],
  val constants: js.UndefOr[js.Array[String]],
  val noXor: js.UndefOr[Boolean],
  val noRelay: js.UndefOr[Boolean],
  val targetType: js.UndefOr[String],
  val tracing: js.UndefOr[Boolean],
  val noEmptyResponse: js.UndefOr[Boolean]
)

object AquaConfig {

  def fromJS(cjs: AquaConfig): ValidatedNec[String, AquaAPIConfig] = {
    cjs.targetType.toOption
      .map(_.toLowerCase())
      .map {
        case "typescript" => validNec(TypeScriptType)
        case "javascript" => validNec(JavaScriptType)
        case "air" => validNec(AirType)
        case _ => invalidNec("Target can be only 'typescript', 'javascript', or 'air'")
      }
      .getOrElse(validNec(AirType))
      .map { target =>
        AquaAPIConfig(
          targetType = target,
          logLevel = cjs.logLevel.getOrElse("info"),
          constants = cjs.constants.map(_.toList).getOrElse(Nil),
          noXor = cjs.noXor.getOrElse(false),
          noRelay = cjs.noRelay.getOrElse(false),
          tracing = cjs.tracing.getOrElse(false),
          noEmptyResponse = cjs.noEmptyResponse.getOrElse(true)
        )
      }
  }
}
