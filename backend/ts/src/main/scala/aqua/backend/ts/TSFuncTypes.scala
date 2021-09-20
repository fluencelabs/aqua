package aqua.backend.ts

import aqua.backend.FuncTypes
import aqua.backend.ts.TypeScriptCommon.{fixupArgName, typeToTs}
import aqua.model.transform.res.FuncRes
import aqua.types.*

case class TSFuncTypes(func: FuncRes) extends FuncTypes {
  import TypeScriptTypes._

  def genTypeName(t: Type, name: String): (Option[String], String) = {
    val genType = typeToTs(t)
    t match {
      case tt: ProductType =>
        val gen = s"type $name = $genType"
        (Some(gen), name)
      case tt: StructType =>
        val gen = s"type $name = $genType"
        (Some(gen), name)
      case _ => (None, genType)

    }
  }

  override val retTypeTs = func.returnType
    .fold((None, "void")) { t => genTypeName(t, func.funcName.capitalize + "Result") }

  override def generate = {
    val configType = "?: {ttl?: number}"

    val argsTypescript = func.args
      .map(arg => s"${typed(fixupArgName(arg.name), typeToTs(arg.`type`))}") :+ s"config$configType"

    // defines different types for overloaded service registration function.
    var funcTypeOverload1 = argsTypescript.mkString(", ")
    var funcTypeOverload2 = (typed("peer", "FluencePeer") :: argsTypescript).mkString(", ")

    val (resTypeDesc, resType) = retTypeTs

    s"""
       |${resTypeDesc.getOrElse("")}
       |export function ${func.funcName}(${funcTypeOverload1}): ${generic("Promise", resType)};
       |export function ${func.funcName}(${funcTypeOverload2}): ${generic("Promise", resType)};""".stripMargin
  }
}
