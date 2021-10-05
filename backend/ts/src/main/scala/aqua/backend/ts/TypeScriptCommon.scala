package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

object TypeScriptCommon {

  def genTypeName(t: Type, name: String): (Option[String], String) = {
    val genType = typeToTs(t)
    t match {
      case tt: ProductType =>
        val gen = s"export type $name = $genType"
        (Some(gen), name)
      case tt: StructType =>
        val gen = s"export type $name = $genType"
        (Some(gen), name)
      case _ => (None, genType)

    }
  }

  def typeToTs(t: Type): String = t match {
    case OptionType(t) => typeToTs(t) + " | null"
    case ArrayType(t) => typeToTs(t) + "[]"
    case StreamType(t) => typeToTs(t) + "[]"
    case pt: ProductType =>
      "[" + pt.toList.map(typeToTs).mkString(", ") + "]"
    case st: StructType => 
      s"{ ${st.fields.map(typeToTs).toNel.map(kv => kv._1 + ": " + kv._2 + ";").toList.mkString(" ")} }"
    case st: ScalarType if ScalarType.number(st) => "number"
    case ScalarType.bool => "boolean"
    case ScalarType.string => "string"
    case lt: LiteralType if lt.oneOf.exists(ScalarType.number) => "number"
    case lt: LiteralType if lt.oneOf(ScalarType.bool) => "boolean"
    case lt: LiteralType if lt.oneOf(ScalarType.string) => "string"
    case _: DataType => "any"
    case at: ArrowType => fnDef(at)
  }

  // TODO: handle cases if there is already peer_ or config_ variable defined
  def fixupArgName(arg: String): String =
    if(arg == "peer" || arg == "config") {
      arg + "_"
    } else {
      arg
    }

  def returnType(at: ArrowType): String =
    at.res.fold("void")(typeToTs)
  
  def fnDef(at: ArrowType): String =
    val args = (argsToTs(at) :+ callParamsArg(at))
      .mkString(", ")
    
    val retType = returnType(at)

    s"(${args}) => ${retType}"

  def argsToTs(at: ArrowType): List[String] =
    FuncRes
      .arrowArgs(at)
      .map(nt => nt.name + ": " + typeToTs(nt.`type`))

  def callParamsArg(at: ArrowType): String =
    val args = FuncRes.arrowArgs(at)
    val generic = if (args.length > 0) {
      val prep = args
        .map(_.name)
        .mkString("' | '")

      "'" + prep + "'"
    } else {
      "null"
    }
    s"callParams: CallParams<${generic}>"

  def callBackExprBody(at: ArrowType, callbackName: String, leftSpace: Int): String = {
    val arrowArgumentsToCallbackArgumentsList =
      at.domain.toList
        .zipWithIndex
        .map((`type`, idx) => {
          val valueFromArg = s"req.args[$idx]"
          `type` match {
            case OptionType(t) => s"${valueFromArg}.length === 0 ? null : ${valueFromArg}[0]"
            case _ => valueFromArg
          }
        })
        .concat(List("callParams"))
        .mkString(", ")

    val callCallbackStatement = s"$callbackName(${arrowArgumentsToCallbackArgumentsList})"
    
    val callCallbackStatementAndReturn =
      at.res.fold(s"${callCallbackStatement}; resp.result = {}")(`type` =>
        `type` match {
          case OptionType(t) => s"""
                                   | var respResult = await ${callCallbackStatement};
                                   | resp.result = respResult === null ? [] : [respResult]
                                   |""".stripMargin
          case _ => s"resp.result = await ${callCallbackStatement}"
        }
      )

    val tetraplets = FuncRes
      .arrowArgs(at)
      .zipWithIndex
      .map((x, idx) => {
        s"${x.name}: req.tetraplets[${idx}]"
      })
      .mkString(",")

    val left = " " * leftSpace

    s"""${left}const callParams = {
       |$left    ...req.particleContext,
       |$left    tetraplets: {
       |$left        ${tetraplets}
       |$left    },
       |$left};
       |${left}resp.retCode = ResultCodes.success;
       |$left${callCallbackStatementAndReturn}""".stripMargin
  }

}
