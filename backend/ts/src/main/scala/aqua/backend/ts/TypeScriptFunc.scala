package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.{ArgDef, FuncCallable}
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.functor._
import cats.syntax.show._

case class TypeScriptFunc(func: FuncCallable) {

  import TypeScriptFunc._

  def argsTypescript: String =
    func.args.args.map(ad => s"${ad.name}: " + typeToTs(ad.`type`)).mkString(", ")

  def generateUniqueArgName(args: List[String], basis: String, attempt: Int): String = {
    val name = if (attempt == 0) {
      basis
    } else {
      basis + attempt
    }
    args.find(_ == name).map(_ => generateUniqueArgName(args, basis, attempt + 1)).getOrElse(name)
  }

  def generateTypescript(conf: BodyConfig = BodyConfig()): String = {

    val tsAir = FuncAirGen(func).generateAir(conf)

    val returnCallback = func.ret.as {
      s"""h.onEvent('${conf.callbackService}', '${conf.respFuncName}', (args) => {
         |  const [res] = args;
         |  resolve(res);
         |});
         |""".stripMargin
    }

    val setCallbacks = func.args.args.map {
      case ArgDef.Data(argName, OptionType(_)) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName === null ? [] : [$argName];});"""
      case ArgDef.Data(argName, _) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName;});"""
      case ArgDef.Arrow(argName, at) =>
        val value = s"$argName(${argsCallToTs(
          at
        )})"
        val expr = at.res.fold(s"$value; return {}")(_ => s"return $value")
        s"""h.on('${conf.callbackService}', '$argName', (args) => {$expr;});"""
    }.mkString("\n")

    val retType = func.ret
      .map(_._2)
      .fold("void")(typeToTs)

    val returnVal =
      func.ret.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val clientArgName = generateUniqueArgName(func.args.args.map(_.name), "client", 0)
    val configArgName = generateUniqueArgName(func.args.args.map(_.name), "config", 0)

    val configType = "{ttl?: number}"

    s"""
       |export async function ${func.funcName}($clientArgName: FluenceClient${if (func.args.isEmpty)
      ""
    else ", "}${argsTypescript}, $configArgName?: $configType): Promise<$retType> {
       |    let request: RequestFlow;
       |    const promise = new Promise<$retType>((resolve, reject) => {
       |        const r = new RequestFlowBuilder()
       |            .disableInjections()
       |            .withRawScript(
       |                `
       |${tsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                ${conf.relayVarName.fold("") { r =>
      s"""h.on('${conf.getDataService}', '$r', () => {
       |                    return $clientArgName.relayPeerId!;
       |                });""".stripMargin
    }}
       |                $setCallbacks
       |                ${returnCallback.getOrElse("")}
       |                h.onEvent('${conf.errorHandlingService}', '${conf.errorFuncName}', (args) => {
       |                    // assuming error is the single argument
       |                    const [err] = args;
       |                    reject(err);
       |                });
       |            })
       |            .handleScriptError(reject)
       |            .handleTimeout(() => {
       |                reject('Request timed out for ${func.funcName}');
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
      s"{${pt.fields.map(typeToTs).toNel.map(kv => kv._1 + ":" + kv._2).toList.mkString(";")}}"
    case st: ScalarType if ScalarType.number(st) => "number"
    case ScalarType.bool => "boolean"
    case ScalarType.string => "string"
    case lt: LiteralType if lt.oneOf.exists(ScalarType.number) => "number"
    case lt: LiteralType if lt.oneOf(ScalarType.bool) => "boolean"
    case lt: LiteralType if lt.oneOf(ScalarType.string) => "string"
    case _: DataType => "any"
    case at: ArrowType =>
      s"(${argsToTs(at)}) => ${at.res
        .fold("void")(typeToTs)}"
  }

  def argsToTs(at: ArrowType): String =
    at.args
      .map(typeToTs)
      .zipWithIndex
      .map(_.swap)
      .map(kv => "arg" + kv._1 + ": " + kv._2)
      .mkString(", ")

  def argsCallToTs(at: ArrowType): String =
    at.args.zipWithIndex.map(_._2).map(idx => s"args[$idx]").mkString(", ")

}
