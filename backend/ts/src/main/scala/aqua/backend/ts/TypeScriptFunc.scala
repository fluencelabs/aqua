package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.{ArgDef, FuncCallable}
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.show._

case class TypeScriptFunc(func: FuncCallable) {

  import TypeScriptCommon._

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
        s"""
           | h.use((req, resp, next) => {
           | if(req.serviceId === '${conf.callbackService}' && req.fnaAme === '$argName') {
           |     ${callBackExprBody(at, argName)}
           | }
           | next();
           | });
        """.stripMargin
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
