package aqua.backend

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypeScriptCommon.fixupArgName
import aqua.backend.ts.{TSFuncTypes, TypeScriptCommon}
import aqua.model.transform.res.FuncRes
import aqua.model.transform.res.FuncRes.Arg
import aqua.types.*
import cats.syntax.show.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputFunc(func: FuncRes, types: Types) {

  import FuncRes.*
  import TypeScriptCommon.*
  import func.*
  import types.*

  val funcTypes = types.funcType(func)
  import funcTypes.*

  def generate: String = {
    val tsAir = FuncAirGen(func).generate
    val codeLeftSpace = " " * 20

    val script = tsAir.show.linesIterator.map(codeLeftSpace + _).mkString("\n")
    val args = func.args.map(a => ArgDefinition.argToDef(a.name, a.`type`))
    val config = func.conf
    val names = NamesConfig(
      config.relayVarName.getOrElse("-relay-"),
      config.getDataService,
      config.callbackService,
      config.callbackService,
      config.respFuncName,
      config.errorHandlingService,
      config.errorFuncName
    )
    val funcDef = FunctionDef(
      func.funcName,
      TypeDefinition(func.returnType),
      args,
      names
    )

    s"""${funcTypes.generate}
       |export function ${func.funcName}(${typed("...args", "any")}) {
       |
       |    let script = `
       |    $script
       |    `
       |    return callFunction(
       |        args,
       |        ${funcDef.asJson.deepDropNullValues.spaces4},
       |        script
       |    )
       |}""".stripMargin
  }

}
