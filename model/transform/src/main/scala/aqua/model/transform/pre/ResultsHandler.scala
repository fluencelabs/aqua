package aqua.model.transform.pre

import aqua.types.Type
import aqua.raw.ops.{Call, CallArrowRawTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}

import cats.syntax.option.*

trait ResultsHandler {
  def handleResults(results: List[(String, Type)]): Option[RawTag.Tree]
}

case class CallbackResultsHandler(callbackSrvId: ValueRaw, funcName: String)
    extends ResultsHandler {

  override def handleResults(results: List[(String, Type)]): Option[RawTag.Tree] =
    if (results.isEmpty) none
    else {
      val resultVars = results.map(VarRaw.apply.tupled)
      val call = Call(
        args = resultVars,
        exportTo = Nil
      )

      CallArrowRawTag.service(callbackSrvId, funcName, call).leaf.some
    }
}
