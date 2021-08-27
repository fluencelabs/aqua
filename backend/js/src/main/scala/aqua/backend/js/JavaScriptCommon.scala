package aqua.backend.js

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

object JavaScriptCommon {

  def fixupArgName(arg: String): String =
    if (arg == "peer" || arg == "config") {
      arg + "_"
    } else {
      arg
    }

  def callBackExprBody(at: ArrowType, callbackName: String): String = {
    val arrowArgumentsToCallbackArgumentsList =
      at.domain.toList.zipWithIndex
        .map(_._2)
        .map(idx => s"req.args[$idx]")
        .concat(List("callParams"))
        .mkString(", ")

    val callCallbackStatement = s"await $callbackName(${arrowArgumentsToCallbackArgumentsList})"

    val callCallbackStatementAndReturn =
      at.res.fold(s"${callCallbackStatement}; resp.result = {}")(_ =>
        s"resp.result = ${callCallbackStatement}"
      )

    val tetraplets = FuncRes
      .arrowArgs(at)
      .zipWithIndex
      .map((x, idx) => {
        s"${x.name}: req.tetraplets[${idx}]"
      })
      .mkString(",")

    s"""
       | const callParams = {
       |     ...req.particleContext,
       |     tetraplets: {
       |         ${tetraplets}
       |     },
       | };
       | resp.retCode = ResultCodes.success;
       | ${callCallbackStatementAndReturn}
       |""".stripMargin
  }

}
