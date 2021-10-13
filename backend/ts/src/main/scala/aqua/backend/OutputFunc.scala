package aqua.backend

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypeScriptCommon.{callBackExprBody, fixupArgName}
import aqua.backend.ts.{TSFuncTypes, TypeScriptCommon}
import aqua.model.transform.res.FuncRes
import aqua.model.transform.res.FuncRes.Arg
import aqua.types.{ArrowType, DataType, OptionType, ProductType}
import cats.syntax.show.*

case class OutputFunc(func: FuncRes, types: Types) {

  import FuncRes.*
  import TypeScriptCommon.*
  import types.*
  import func.*
  val funcTypes = types.funcType(func)
  import funcTypes.*

  def generate: String = {
    val tsAir = FuncAirGen(func).generate
    val codeLeftSpace = " " * 20

    s"""${funcTypes.generate}
       |export function ${func.funcName}(${typed("...args", "any")}) {
       |    return callFunction({
       |        rawFnArgs: args,
       |        idVoid: false,
       |        args: [],
       |        functionName: '${func.funcName}',
       |        names: {
       |            relay: '-relay-',
       |            getDataSrv: 'getDataSrv',
       |            callbackSrv: 'callbackSrv',
       |            responseSrv: 'response',
       |            errorHandlingSrv: 'errorHandlingSrv',
       |            errorFnName: 'error',
       |            responseFnName: 'response',
       |        },
       |        script: `${tsAir.show.linesIterator.map(codeLeftSpace + _).mkString("\n")}`
       |    })
       |}""".stripMargin
  }

}
