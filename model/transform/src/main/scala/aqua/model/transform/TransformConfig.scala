package aqua.model.transform

import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
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
  constants: List[TransformConfig.Const] = Nil
) {

  private val quote = LiteralRaw.quote andThen LiteralModel.fromRaw

  val errorId: ValueModel = quote(errorFuncName)
  val errorHandlingCallback: ValueModel = quote(errorHandlingService)
  val callbackSrvId: ValueModel = quote(callbackService)
  val dataSrvId: ValueModel = quote(getDataService)

  // Host peer id holds %init_peer_id% in case Aqua is not compiled to be executed behind a relay,
  // or relay's variable otherwise
  val hostPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "HOST_PEER_ID",
      relayVarName.fold[ValueModel](LiteralModel.fromRaw(ValueRaw.InitPeerId))(r =>
        VarModel(r, ScalarType.string)
      )
    )

  val initPeerId: TransformConfig.Const =
    TransformConfig.Const(
      "INIT_PEER_ID",
      LiteralModel.fromRaw(ValueRaw.InitPeerId)
    )

  val nil: TransformConfig.Const =
    TransformConfig.Const(
      "nil", // TODO: shouldn't it be NIL?
      LiteralModel.fromRaw(ValueRaw.Nil)
    )

  val lastError: TransformConfig.Const =
    TransformConfig.Const(
      "LAST_ERROR",
      VarModel(ValueRaw.LastError.name, ValueRaw.LastError.`type`, Chain.empty)
    )

  val constantsMap: Map[String, ValueModel] =
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
}
