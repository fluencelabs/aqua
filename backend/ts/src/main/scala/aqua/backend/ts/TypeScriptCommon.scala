package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

object TypeScriptCommon {

  def typeToTs(t: Type): String = t match {
    case OptionType(t) => typeToTs(t) + " | null"
    case ArrayType(t) => typeToTs(t) + "[]"
    case StreamType(t) => typeToTs(t) + "[]"
    case pt: ProductType =>
      "[" + pt.toList.map(typeToTs).mkString(", ") + "]"
    case st: StructType => 
      s"{${st.fields.map(typeToTs).toNel.map(kv => kv._1 + ":" + kv._2).toList.mkString(";")}}"
    case st: ScalarType if ScalarType.number(st) => "number"
    case ScalarType.bool => "boolean"
    case ScalarType.string => "string"
    case lt: LiteralType if lt.oneOf.exists(ScalarType.number) => "number"
    case lt: LiteralType if lt.oneOf(ScalarType.bool) => "boolean"
    case lt: LiteralType if lt.oneOf(ScalarType.string) => "string"
    case _: DataType => "any"
    case at: ArrowType => fnDef(at)
  }

  def returnType(at: ArrowType): String =
    at.res.fold("void")(typeToTs)

  def fnDef(at: ArrowType): String =
    val args = argsToTs(at)
      .concat(List(callParamsArg(at)))
      .mkString(", ")

    s"(${args}) => ${returnType(at)}"

  def argsToTs(at: ArrowType): List[String] =
    FuncRes
      .arrowArgs(at)
      .map(nt => nt.name + ": " + typeToTs(nt.`type`))

  def callParamsArg(at: ArrowType): String =
    val args = FuncRes.arrowArgs(at)
    var generic = if (args.length > 0) {
      val prep = args
        .map(x => {
          x.name
        })
        .mkString("' | '")

      "'" + prep + "'"
    } else {
      "null"
    }
    s"callParams: CallParams<${generic}>"

  def argsCallToTs(at: ArrowType): List[String] =
    FuncRes.arrowArgIndices(at).map(idx => s"args[$idx]")

  def callBackExprBody(at: ArrowType, callbackName: String): String = {
    val arrowArgumentsToCallbackArgumentsList =
      at.domain.toList
        .zipWithIndex
        .map(_._2)
        .map(idx => s"req.args[$idx]")
        .concat(List("callParams"))
        .mkString(", ")

    val callCallbackStatement = s"$callbackName(${arrowArgumentsToCallbackArgumentsList})"
    
    val callCallbackStatementAndReturn =
      at.res.fold(s"${callCallbackStatement}; resp.result = {}")(_ =>
        s"resp.result = ${callCallbackStatement}"
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
