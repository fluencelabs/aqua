package aqua.backend.ts

import aqua.backend.FuncTypes
import aqua.backend.ts.TypeScriptCommon.{fixupArgName, genTypeName, typeToTs}
import aqua.model.transform.res.FuncRes
import aqua.types.*

case class TSFuncTypes(func: FuncRes) extends FuncTypes {
  import TypeScriptTypes.*

  override val retTypeTs = func.returnType
    .fold((None, "void")) { t => genTypeName(t, func.funcName.capitalize + "Result") }

  override def generate = {
    val configType = "?: {ttl?: number}"

    val argsTypescript = func.args
      .map { arg =>
        val (typeDesc, t) = genTypeName(arg.`type`, func.funcName.capitalize + "Arg" + arg.name.capitalize)
        (typeDesc, s"${typed(fixupArgName(arg.name), t)}")
      } :+ (None, s"config$configType")

    val args = argsTypescript.map(a => "    " + a._2)
    val argsDesc = argsTypescript.flatMap(_._1)

    // defines different types for overloaded service registration function.
    val funcTypeOverload1 = args.mkString(",\n")
    val funcTypeOverload2 = (("    " + typed("peer", "FluencePeer")) :: args).mkString(",\n")

    val (resTypeDesc, resType) = retTypeTs

    s"""${argsDesc.mkString("\n")} 
       |${resTypeDesc.getOrElse("")}
       |export function ${func.funcName}(
       |${funcTypeOverload1}
       |): ${generic("Promise", resType)};
       |
       |export function ${func.funcName}(
       |${funcTypeOverload2}
       |): ${generic("Promise", resType)};
       |""".stripMargin
  }
}
