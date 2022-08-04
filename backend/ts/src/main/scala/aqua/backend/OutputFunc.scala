package aqua.backend

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypeScriptCommon.fixupArgName
import aqua.backend.ts.{TSFuncTypes, TypeScriptCommon}
import aqua.res.FuncRes
import aqua.types.*
import cats.data.ValidatedNec
import cats.syntax.show.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputFunc(
  func: FuncRes,
  types: Types,
  airChecker: String => ValidatedNec[String, Unit]
) {

  import FuncRes.*
  import TypeScriptCommon.*
  import func.*
  import types.*

  val funcTypes = types.funcType(func)

  import funcTypes.*
  import TypeDefinition.*

  def generate: ValidatedNec[String, String] = {
    val tsAir = FuncAirGen(func).generate
    val codeLeftSpace = " " * 20

    val script = tsAir.show.linesIterator.map(codeLeftSpace + _).mkString("\n")

    val funcDef = FunctionDef(func)

    airChecker(script).map(_ => s"""${funcTypes.generate}
                                   |export function ${func.funcName}(${typed("...args", "any")}) {
                                   |
                                   |    let script = `
                                   |$script
                                   |    `
                                   |    return callFunction(
                                   |        args,
                                   |        ${funcDef.asJson.deepDropNullValues.spaces4},
                                   |        script
                                   |    )
                                   |}""".stripMargin)
  }

}
