package aqua.backend

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypeScriptCommon.{callBackExprBody, fixupArgName}
import aqua.backend.ts.{TSFuncTypes, TypeScriptCommon}
import aqua.model.transform.res.FuncRes
import aqua.model.transform.res.FuncRes.Arg
import aqua.types.*
import cats.syntax.show.*

import scala.io.circe.*
import scala.io.circe.generic.auto.*
import scala.io.circe.parser.*
import scala.io.circe.syntax.*

case class OutputFunc(func: FuncRes, types: Types) {

  import FuncRes.*
  import TypeScriptCommon.*
  import func.*
  import types.*
  val funcTypes = types.funcType(func)
  import funcTypes.*

  def argToDef(name: String, `type`: Type, isOptional: Boolean = false): ArgDef = {
    `type` match {
      case OptionType(t) =>
        argToDef(name, t, isOptional = true)
      case a@ArrowType(_, _) =>
        val callbackDef = CallbackDef(a)
        ArgDef(name, isOptional, Some(callbackDef))
      case _ => ArgDef(name, isOptional, None)
    }
  }

  def generate: String = {
    val tsAir = FuncAirGen(func).generate
    val codeLeftSpace = " " * 20

    val script = tsAir.show.linesIterator.map(codeLeftSpace + _).mkString("\n")
    val args = func.args.map(a => argToDef(a.name, a.`type`))
    val config = func.conf
    val names = Names(
      config.relayVarName.getOrElse("-relay-"),
      config.getDataService,
      config.callbackService,
      config.respFuncName,
      config.errorHandlingService,
      config.errorFuncName,
      config.respFuncName
    )
    val funcDef = FunctionCallDef(func.funcName, func.returnType.isEmpty, args, names)

    s"""${funcTypes.generate}
       |export function ${func.funcName}(${typed("...args", "any")}) {
       |
       |    let script = `
       |    $script
       |    `
       |    return callFunction(
       |        args,
       |        ${funcDef.asJson.spaces4},
       |        script
       |    )
       |}""".stripMargin
  }

}
