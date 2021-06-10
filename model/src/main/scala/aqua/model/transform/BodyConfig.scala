package aqua.model.transform

import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import cats.kernel.Monoid

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

  // TODO: add constants to BodyConfig, and register there
  implicit val aquaContextMonoid: Monoid[AquaContext] =
    AquaContext
      .implicits(
        AquaContext.blank.copy(values = Map(VarModel.lastError.name -> VarModel.lastError))
      )
      .aquaContextMonoid

}
