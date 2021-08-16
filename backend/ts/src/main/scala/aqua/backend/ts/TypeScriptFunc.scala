package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

case class TypeScriptFunc(func: FuncRes) {

  import TypeScriptFunc._
  import FuncRes._
  import func._

  def argsTypescript: String =
    args.map(ad => s"${ad.name}: " + typeToTs(ad.`type`)).mkString(", ")

  private def returnCallback: String = returnType.fold("") { retType =>
    val respBody = retType match {
      case OptionType(_) =>
        """  let [opt] = args;
          |  if (Array.isArray(opt)) {
          |      if (opt.length === 0) { resolve(null); }
          |      opt = opt[0];
          |  }          
          |  return resolve(opt);""".stripMargin
      case pt: ProductType =>
        val unwrapOpts = pt.toList.zipWithIndex.collect { case (OptionType(_), i) =>
          s"""
             |  if(Array.isArray(opt[$i])) {
             |     if (opt[$i].length === 0) { opt[$i] = null; }
             |     else {opt[$i] = opt[$i][0]; }
             |  }""".stripMargin
        }.mkString

        s""" let opt: any = args;
           |$unwrapOpts
           | return resolve(opt);""".stripMargin
      case _ =>
        """  const [res] = args;
          |  resolve(res);""".stripMargin

    }
    s"""h.onEvent('$callbackServiceId', '$respFuncName', (args) => {
       |  $respBody
       |});
       |""".stripMargin
  }

  def generate: String = {

    val tsAir = FuncAirGen(func).generate

    val retTypeTs = func.returnType
      .fold("void")(typeToTs)

    val setCallbacks = func.args.collect { // Product types are not handled
      case Arg(argName, OptionType(_)) =>
        s"""h.on('$dataServiceId', '$argName', () => {return $argName === null ? [] : [$argName];});"""
      case Arg(argName, _: DataType) =>
        s"""h.on('$dataServiceId', '$argName', () => {return $argName;});"""
      case Arg(argName, at: ArrowType) =>
        val value = s"$argName(${argsCallToTs(
          at
        )})"
        val expr = arrowToRes(at).fold(s"$value; return {}")(_ => s"return $value")
        s"""h.on('$callbackServiceId', '$argName', (args) => {$expr;});"""
    }
      .mkString("\n")

    val returnVal =
      returnType.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val clientArgName = genArgName("client")
    val configArgName = genArgName("config")

    val configType = "{ttl?: number}"

    s"""
       |export async function ${funcName}($clientArgName: FluenceClient${if (args.isEmpty)
      ""
    else ", "}${argsTypescript}, $configArgName?: $configType): Promise<$retTypeTs> {
       |    let request: RequestFlow;
       |    const promise = new Promise<$retTypeTs>((resolve, reject) => {
       |        const r = new RequestFlowBuilder()
       |            .disableInjections()
       |            .withRawScript(
       |                `
       |${tsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                ${relayVarName.fold("") { r =>
      s"""h.on('$dataServiceId', '$r', () => {
       |                    return $clientArgName.relayPeerId!;
       |                });""".stripMargin
    }}
       |                $setCallbacks
       |                $returnCallback
       |                h.onEvent('$errorHandlerId', '$errorFuncName', (args) => {
       |                    // assuming error is the single argument
       |                    const [err] = args;
       |                    reject(err);
       |                });
       |            })
       |            .handleScriptError(reject)
       |            .handleTimeout(() => {
       |                reject('Request timed out for ${funcName}');
       |            })
       |        if(${configArgName} && ${configArgName}.ttl) {
       |            r.withTTL(${configArgName}.ttl)
       |        }
       |        request = r.build();
       |    });
       |    await $clientArgName.initiateFlow(request!);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}

object TypeScriptFunc {

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
    case at: ArrowType =>
      s"(${argsToTs(at)}) => ${FuncRes
        .arrowToRes(at)
        .fold("void")(typeToTs)}"
  }

  def argsToTs(at: ArrowType): String =
    FuncRes
      .arrowArgs(at)
      .map(nt => nt.name + ": " + typeToTs(nt.`type`))
      .mkString(", ")

  def argsCallToTs(at: ArrowType): String =
    FuncRes.arrowArgIndices(at).map(idx => s"args[$idx]").mkString(", ")

}
