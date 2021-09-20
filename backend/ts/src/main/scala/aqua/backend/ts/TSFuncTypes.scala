package aqua.backend.ts

import aqua.backend.FuncTypes
import aqua.backend.ts.TypeScriptCommon.{fixupArgName, typeToTs, genTypeName}
import aqua.model.transform.res.FuncRes
import aqua.types.*

case class TSFuncTypes(func: FuncRes) extends FuncTypes {
  import TypeScriptTypes._

  override val retTypeTs = func.returnType
    .fold((None, "void")) { t => genTypeName(t, func.funcName.capitalize + "Result") }

  override def generate = {
    val configType = "?: {ttl?: number}"

    val argsTypescript = func.args
      .map { arg =>
        val (typeDesc, t) = genTypeName(arg.`type`, func.funcName.capitalize + "Arg" + arg.name.capitalize)
        (typeDesc, s"${typed(fixupArgName(arg.name), t)}")
      } :+ (None, s"config$configType")

    val args = argsTypescript.map(_._2)
    val argsDesc = argsTypescript.map(_._1).flatten

    // defines different types for overloaded service registration function.
    var funcTypeOverload1 = args.mkString(", ")
    var funcTypeOverload2 = (typed("peer", "FluencePeer") :: args).mkString(", ")

    val (resTypeDesc, resType) = retTypeTs

    s"""${argsDesc.mkString("\n")} 
       |${resTypeDesc.getOrElse("")}
       |export function ${func.funcName}(${funcTypeOverload1}): ${generic("Promise", resType)};
       |export function ${func.funcName}(${funcTypeOverload2}): ${generic("Promise", resType)};""".stripMargin
  }
}
