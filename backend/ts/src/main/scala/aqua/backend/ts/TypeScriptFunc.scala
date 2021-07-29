package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.{ArgDef, FuncCallable}
import aqua.model.transform.BodyConfig
import aqua.types._
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

  private def genReturnCallback(
    retType: Type,
    callbackService: String,
    respFuncName: String
  ): String = {
    val body = retType match {
      case OptionType(_) =>
        """  let [opt] = args;
          |  if (Array.isArray(opt)) {
          |      if (opt.length === 0) { resolve(null); }
          |      opt = opt[0];
          |  }
          |  return resolve(opt);""".stripMargin
      case _ =>
        """  const [res] = args;
          |  resolve(res);""".stripMargin

    }
    s"""h.onEvent('$callbackService', '$respFuncName', (args) => {
       |  $body
       |});
       |""".stripMargin
  }

  def generateTypescript(conf: BodyConfig = BodyConfig()): String = {

    val tsAir = FuncAirGen(func).generateAir(conf)

    val retType = func.ret
      .map(_._2)
      .fold("void")(typeToTs)

    val returnCallback = func.ret
      .map(_._2)
      .map(t => genReturnCallback(t, conf.callbackService, conf.respFuncName))
      .getOrElse("")

    val setCallbacks = func.args.args.map {
      case ArgDef.Data(argName, OptionType(_)) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName === null ? [] : [$argName];});"""
      case ArgDef.Data(argName, _) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName;});"""
      case ArgDef.Arrow(argName, at) =>
        val callCallbackStatement = s"$argName(${argsCallToTsForUseStatement(
          at
        )})"
        val callCallbackStatementAndReturn =
          at.res.fold(s"${callCallbackStatement}; resp.result = {}")(_ =>
            s"resp.result = ${callCallbackStatement}"
          )
        s"""
         | h.use((req, resp, next) => {
         | if(req.serviceId === '${conf.callbackService}' && req.fnaAme === '$argName') {
         | const callParams = {
         |         ...req.particleContext,
         |         tetraplets: {
         |             arg0: req.tetraplets[0],
         |             arg1: req.tetraplets[1],
         |         },
         |     };
         |     resp.retCode = ResultCodes.success;
         |     const res = ${callCallbackStatementAndReturn}
         |     resp.result = {};
         | }
         | next();
         | }) 
        """
    }.mkString("\n")

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
       |                $returnCallback
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

  def argsCallToTsForUseStatement(at: ArrowType): String =
    at.args.zipWithIndex
      .map(_._2)
      .map(idx => s"req.args[$idx]")
      .concat(List("callParams"))
      .mkString(", ")

  def argsCallToTsForTetraplets(at: ArrowType): String =
    at.args.zipWithIndex
      .map(_._2)
      .map(idx => s"arg${idx}: req.tetraplets[${idx}]]")
      .mkString("," + System.lineSeparator)

}
