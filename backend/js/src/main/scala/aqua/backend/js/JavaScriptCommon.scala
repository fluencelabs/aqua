package aqua.backend.js

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

object JavaScriptCommon {

  // TODO: handle cases if there is already peer_ or config_ variable defined
  def fixupArgName(arg: String): String =
    if (arg == "peer" || arg == "config") {
      arg + "_"
    } else {
      arg
    }

  def callBackExprBody(at: ArrowType, callbackName: String): String = {
    val arrowArgumentsToCallbackArgumentsList =
      at.domain.toList.zipWithIndex
        .map((`type`, idx) => {
          val valueFromArg = s"req.args[$idx]"
          `type` match {
            case OptionType(t) =>
              s"${valueFromArg}.length === 0 ? null : ${valueFromArg}[0]"
            case _ => valueFromArg
          }
        })
        .concat(List("callParams"))
        .mkString(", ")

    val callCallbackStatement = s"$callbackName($arrowArgumentsToCallbackArgumentsList)"

    val callCallbackStatementAndReturn =
      at.res.fold(s"${callCallbackStatement}; resp.result = {}")(`type` =>
        `type` match {
          case OptionType(t) => s"""
                                   | var respResult = ${callCallbackStatement};
                                   | resp.result = respResult === null ? [] : [respResult]
                                   |""".stripMargin
          case _ => s"resp.result = ${callCallbackStatement}"
        }
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
