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

  val errorId: ValueModel = LiteralModel.quote(errorFuncName)
  val errorHandlingCallback: ValueModel = LiteralModel.quote(errorHandlingService)
  val callbackSrvId: ValueModel = LiteralModel.quote(callbackService)
  val dataSrvId: ValueModel = LiteralModel.quote(getDataService)

}
