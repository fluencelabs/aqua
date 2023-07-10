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

  val errorId: ValueRaw = LiteralRaw.quote(errorFuncName)
  val errorHandlingSrvId: ValueRaw = LiteralRaw.quote(errorHandlingService)
  val callbackSrvId: ValueRaw = LiteralRaw.quote(callbackService)
  val dataSrvId: ValueRaw = LiteralRaw.quote(getDataService)

  val constantsList: List[ConstantRaw] =
    ConstantRaw.defaultConstants(relayVarName) ::: constants
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
