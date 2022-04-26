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
  constants: List[ConstantRaw] = Nil
) {

  import LiteralRaw.quote

  val errorId: ValueRaw = quote(errorFuncName)
  val errorHandlingCallback: ValueModel = LiteralModel fromRaw quote(errorHandlingService)
  val callbackSrvId: ValueRaw = quote(callbackService)
  val dataSrvId: ValueRaw = quote(getDataService)

  // Host peer id holds %init_peer_id% in case Aqua is not compiled to be executed behind a relay,
  // or relay's variable otherwise
  val hostPeerId: ConstantRaw =
    ConstantRaw(
      "HOST_PEER_ID",
      relayVarName.fold[ValueRaw](ValueRaw.InitPeerId)(r => VarRaw(r, ScalarType.string)),
      false
    )

  val initPeerId: ConstantRaw =
    ConstantRaw(
      "INIT_PEER_ID",
      ValueRaw.InitPeerId,
      false
    )

  val particleTtl: ConstantRaw =
    ConstantRaw(
      "PARTICLE_TTL",
      ValueRaw.ParticleTtl,
      false
    )

  val particleTimeout: ConstantRaw =
    ConstantRaw(
      "PARTICLE_TIMEOUT",
      ValueRaw.ParticleTimeout,
      false
    )

  val nil: ConstantRaw =
    ConstantRaw(
      "nil", // TODO: shouldn't it be NIL?
      ValueRaw.Nil,
      false
    )

  val lastError: ConstantRaw =
    ConstantRaw(
      "LAST_ERROR",
      ValueRaw.LastError,
      false
    )

  val constantsList: List[ConstantRaw] =
    hostPeerId :: initPeerId :: particleTtl :: particleTimeout nil :: lastError :: constants
}
