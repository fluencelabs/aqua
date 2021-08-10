package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.{ArgDef, FuncCallable}
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.show._

object TypeScriptCommon {

  def typeToTs(t: Type): String = t match {
    case OptionType(t) => typeToTs(t) + " | null"
    case ArrayType(t) => typeToTs(t) + "[]"
    case StreamType(t) => typeToTs(t) + "[]"
    case pt: ProductType =>
      s"{${pt.fields.map(typeToTs).toNel.map(kv => kv._1 + ":" + kv._2).toList.mkString(";")}}"
    case st: ScalarType if ScalarType.number(st) => "number"
    case ScalarType.bool => "boolean"
    case ScalarType.string => "string"
    case lt: LiteralType if lt.oneOf.exists(ScalarType.number) => "number"
    case lt: LiteralType if lt.oneOf(ScalarType.bool) => "boolean"
    case lt: LiteralType if lt.oneOf(ScalarType.string) => "string"
    case _: DataType => "any"
    case at: ArrowType => fnBodyDef(at)
  }

  def callBackExprBody(at: ArrowType, callbackName: String): String = {
    val callCallbackStatement = s"$callbackName(${arrowArgumentsToCallbackArgumentsList(
      at
    )})"
    val callCallbackStatementAndReturn =
      at.res.fold(s"${callCallbackStatement}; resp.result = {}")(_ =>
        s"resp.result = ${callCallbackStatement}"
      )

    s"""
       | const callParams = {
       |     ...req.particleContext,
       |     tetraplets: {
       |         ${arrowArgumentsToTetrapletsStructure(at)}
       |     },
       | };
       | resp.retCode = ResultCodes.success;
       | ${callCallbackStatementAndReturn}
       |""".stripMargin
  }

  def arrowArgumentsToTsArgumentList(at: ArrowType): String =
    at.args
      .map(typeToTs)
      .zipWithIndex
      .map(_.swap)
      .map(kv => "arg" + kv._1 + ": " + kv._2)
      .concat(List("callParams: CallParams"))
      .mkString(", ")

  def arrowArgumentsToCallbackArgumentsList(at: ArrowType): String =
    at.args.zipWithIndex
      .map(_._2)
      .map(idx => s"req.args[$idx]")
      .concat(List("callParams"))
      .mkString(", ")

  def arrowArgumentsToTetrapletsStructure(at: ArrowType): String =
    at.args.zipWithIndex
      .map(_._2)
      .map(idx => s"arg${idx}: req.tetraplets[${idx}]")
      .mkString("," + System.lineSeparator())

  def fnBodyDef(arrow: ArrowType) = {
    val argsWithTypes = arrow.args
      .map(typeToTs)
      .zipWithIndex
      .map(_.swap)
      .map(kv => ("arg" + kv._1, kv._2))

    val callParamsGeneric = if (argsWithTypes.length > 0) {
      val prep = argsWithTypes
        .map(kv => kv._1)
        .mkString("' | '")

      "'" + prep + "'"
    } else {
      "null"
    }

    val args = argsWithTypes
      .map(kv => kv._1 + ": " + kv._2)
      .concat(List(s"callParams: CallParams<${callParamsGeneric}>"))
      .mkString(", ")

    s"(${args}) => ${arrow.res.fold("void")(typeToTs)}"
  }

}
