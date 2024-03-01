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
