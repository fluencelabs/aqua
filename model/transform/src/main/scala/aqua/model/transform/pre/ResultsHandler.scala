package aqua.model.transform.pre

import aqua.raw.ops.{Call, CallArrowRawTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.Type

import cats.syntax.option.*

trait ResultsHandler {
  def handleResults(results: List[(String, Type)]): Option[RawTag.Tree]
}

case class CallbackResultsHandler(
  callbackSrvId: ValueRaw,
  funcName: String,
  noEmptyResponse: Boolean
) extends ResultsHandler {

  override def handleResults(results: List[(String, Type)]): Option[RawTag.Tree] =
    if (results.isEmpty && noEmptyResponse) none
    else {
      val resultVars = results.map(VarRaw.apply.tupled)
      val call = Call(
        args = resultVars,
        exportTo = Nil
      )

      CallArrowRawTag.service(callbackSrvId, funcName, call).leaf.some
    }
}
