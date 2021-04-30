package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel}

case class BodyConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  errorHandlingService: String = "errorHandlingSrv",
  errorFuncName: String = "error",
  respFuncName: String = "response",
  relayVarName: Option[String] = Some("-relay-"),
  wrapWithXor: Boolean = true
) {

  val errorId: ValueModel = LiteralModel("\"" + errorFuncName + "\"")
  val errorHandlingCallback: ValueModel = LiteralModel("\"" + errorHandlingService + "\"")
  val callbackSrvId: ValueModel = LiteralModel("\"" + callbackService + "\"")
  val dataSrvId: ValueModel = LiteralModel("\"" + getDataService + "\"")

}
