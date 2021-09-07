package aqua.model.transform

import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import aqua.types.ScalarType
import cats.kernel.Monoid

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

  val errorId: ValueModel = LiteralModel.quote(errorFuncName)
  val errorHandlingCallback: ValueModel = LiteralModel.quote(errorHandlingService)
  val callbackSrvId: ValueModel = LiteralModel.quote(callbackService)
  val dataSrvId: ValueModel = LiteralModel.quote(getDataService)

  // Host peer id holds %init_peer_id% in case Aqua is not compiled to be executed behind a relay,
  // or relay's variable otherwise
  val hostPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "HOST_PEER_ID",
      relayVarName.fold[ValueModel](LiteralModel.initPeerId)(r => VarModel(r, ScalarType.string))
    )

  val initPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "INIT_PEER_ID",
      LiteralModel.initPeerId
    )

  val nil: TransformConfig.Const =
    TransformConfig.Const(
      "nil", // TODO: shouldn't it be NIL?
      LiteralModel.nil
    )

  val lastError: TransformConfig.Const =
    TransformConfig.Const(
      "LAST_ERROR",
      VarModel.lastError
    )

  val constantsMap =
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
  case class Const(name: String, value: ValueModel)

  def forHost: TransformConfig =
    TransformConfig(wrapWithXor = false, relayVarName = None)
}
