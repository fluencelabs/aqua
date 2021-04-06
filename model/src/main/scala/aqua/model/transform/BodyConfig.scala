package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel}

case class BodyConfig(
  getDataService: String = "getDataSrv",
  callbackService: String = "callbackSrv",
  respFuncName: String = "response",
  relayVarName: String = "relay"
) {

  val callbackSrvId: ValueModel = LiteralModel("\"" + callbackService + "\"")
  val dataSrvId: ValueModel = LiteralModel("\"" + getDataService + "\"")

}
