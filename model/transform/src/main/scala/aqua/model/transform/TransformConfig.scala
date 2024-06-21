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

package aqua.model.transform

import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.raw.{ConstantRaw, RawContext, RawPart}
import aqua.types.ScalarType

import cats.data.Chain
import cats.kernel.Monoid

/**
 * Configuration for function pre transformer
 *
 * @param getDataService - name of the service that provides arguments
 * @param callbackService - name of the service that provides callbacks
 * @param errorHandlingService - name of the service that handles errors
 * @param errorFuncName - name of the function that handles errors (in errorHandlingService)
 * @param respFuncName - name of the function that handles responses (in getDataService)
 * @param noEmptyResponse - if true, do not generate response call if there is no return values
 * @param relayVarName - name of the relay variable
 * @param tracing - tracing configuration
 * @param constants - list of constants
 * @param noXor - if true, do not generate generate `xor`s for error propagation
 */
case class TransformConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  errorHandlingService: String = "errorHandlingSrv",
  errorFuncName: String = "error",
  respFuncName: String = "response",
  noEmptyResponse: Boolean = false,
  relayVarName: Option[String] = Some("-relay-"),
  tracing: Option[TransformConfig.TracingConfig] = None,
  noXor: Boolean = false
) {
  val errorId: ValueRaw = LiteralRaw.quote(errorFuncName)
  val errorHandlingSrvId: ValueRaw = LiteralRaw.quote(errorHandlingService)
  val callbackSrvId: ValueRaw = LiteralRaw.quote(callbackService)
  val dataSrvId: ValueRaw = LiteralRaw.quote(getDataService)
}

object TransformConfig {

  final case class TracingConfig(
    serviceId: String = "tracingSrv",
    serviceFuncName: String = "tracingEvent"
  )

  object TracingConfig {
    lazy val default = TracingConfig()
  }
}
