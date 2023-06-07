package aqua.model.transform

import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import aqua.raw.{ConstantRaw, RawContext, RawPart}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.ScalarType
import cats.data.Chain
import cats.kernel.Monoid

// TODO docs
case class TransformConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  errorHandlingService: String = "errorHandlingSrv",
  errorFuncName: String = "error",
  respFuncName: String = "response",
  relayVarName: Option[String] = Some("-relay-"),
  wrapWithXor: Boolean = true,
  tracing: Option[TransformConfig.TracingConfig] = None,
  constants: List[ConstantRaw] = Nil
) {

  import LiteralRaw.quote

  val errorId: ValueRaw = quote(errorFuncName)
  val errorHandlingCallback: ValueModel = LiteralModel fromRaw quote(errorHandlingService)
  val callbackSrvId: ValueRaw = quote(callbackService)
  val dataSrvId: ValueRaw = quote(getDataService)

  val constantsList: List[ConstantRaw] =
    ConstantRaw.defaultConstants(relayVarName) ::: constants
}

object TransformConfig {

  final case class TracingConfig(
    serviceId: String = "tracingSrv",
    serviceFuncName: String = "tracingEvent"
  )
}
