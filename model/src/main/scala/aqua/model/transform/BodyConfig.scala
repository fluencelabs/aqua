package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel}
import aqua.types.LiteralType

case class BodyConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  errorHandlingService: String = "errorHandlingSrv",
  errorFuncName: String = "error",
  respFuncName: String = "response",
  relayVarName: Option[String] = Some("-relay-"),
  wrapWithXor: Boolean = true
) {

  val errorId: ValueModel = LiteralModel("\"" + errorFuncName + "\"", LiteralType.string)

  val errorHandlingCallback: ValueModel =
    LiteralModel("\"" + errorHandlingService + "\"", LiteralType.string)
  val callbackSrvId: ValueModel = LiteralModel("\"" + callbackService + "\"", LiteralType.string)
  val dataSrvId: ValueModel = LiteralModel("\"" + getDataService + "\"", LiteralType.string)

}
