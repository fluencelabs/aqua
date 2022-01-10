package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.raw.AquaContext
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.ScalarType
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
  constants: List[TransformConfig.Const] = Nil
) {

  val errorId: ValueRaw = LiteralRaw.quote(errorFuncName)
  val errorHandlingCallback: ValueRaw = LiteralRaw.quote(errorHandlingService)
  val callbackSrvId: ValueRaw = LiteralRaw.quote(callbackService)
  val dataSrvId: ValueRaw = LiteralRaw.quote(getDataService)

  // Host peer id holds %init_peer_id% in case Aqua is not compiled to be executed behind a relay,
  // or relay's variable otherwise
  val hostPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "HOST_PEER_ID",
      relayVarName.fold[ValueRaw](ValueRaw.InitPeerId)(r => VarRaw(r, ScalarType.string))
    )

  val initPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "INIT_PEER_ID",
      ValueRaw.InitPeerId
    )

  val nil: TransformConfig.Const =
    TransformConfig.Const(
      "nil", // TODO: shouldn't it be NIL?
      ValueRaw.Nil
    )

  val lastError: TransformConfig.Const =
    TransformConfig.Const(
      "LAST_ERROR",
      ValueRaw.LastError
    )

  val constantsMap: Map[String, ValueRaw] =
    (hostPeerId :: initPeerId :: nil :: lastError :: constants)
      .map(c => c.name -> c.value)
      .toMap

  implicit val aquaContextMonoid: Monoid[AquaContext] = {

    AquaContext
      .implicits(
        AquaContext.blank
          .copy(values = constantsMap)
      )
      .aquaContextMonoid
  }

}

object TransformConfig {
  case class Const(name: String, value: ValueRaw)

  // TODO docs/rename? why it is unused
  def forHost: TransformConfig =
    TransformConfig(wrapWithXor = false, relayVarName = None)
}
