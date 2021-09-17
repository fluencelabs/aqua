package aqua.backend.ts

import aqua.backend.FuncTypes
import aqua.backend.ts.TypeScriptCommon.{fixupArgName, typeToTs, typeToTsAdd}
import aqua.model.transform.res.FuncRes

case class TSFuncTypes(func: FuncRes) extends FuncTypes {
  import TypeScriptTypes._

  override lazy val retTypeTs = func.returnType
    .fold("<void>")(t => s"<${typeToTs(t)}>")

  override lazy val exportTypes = {
    val configType = "?: {ttl?: number}"

    val argsTypescript = func.args
      .map(arg => s"${fixupArgName(arg.name)}${typeToTsAdd(arg.`type`)}") :+ s"config$configType"

    // defines different types for overloaded service registration function.
    var funcTypeOverload1 = argsTypescript.mkString(", ")
    var funcTypeOverload2 = ("peer: FluencePeer" :: argsTypescript).mkString(", ")

    val funcTypeRes = s"Promise$retTypeTs"

    s"""export function ${func.funcName}(${funcTypeOverload1}) : ${funcTypeRes};
       |export function ${func.funcName}(${funcTypeOverload2}) : ${funcTypeRes};""".stripMargin
  }

  def generate = exportTypes
}
