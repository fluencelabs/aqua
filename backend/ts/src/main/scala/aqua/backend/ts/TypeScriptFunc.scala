package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

case class TypeScriptFunc(func: FuncRes) {

  import TypeScriptCommon._
  import FuncRes._
  import func._

  private def returnCallback: String = 
    val respBody = func.returnType match {
      case Some(x) => x match {
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
      case None => ""
    }
    s"""h.onEvent('$callbackServiceId', '$respFuncName', (args) => {
       |  $respBody
       |});
       |""".stripMargin

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
        s"""
           | h.use((req, resp, next) => {
           | if(req.serviceId === '${conf.callbackService}' && req.fnaAme === '$argName') {
           |     ${callBackExprBody(at, argName)}
           | }
           | next();
           | });
        """.stripMargin
    }
      .mkString("\n")

    val returnVal =
      func.returnType.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val clientArgName = genArgName("client")
    val configArgName = genArgName("config")

    val configType = "{ttl?: number}"

    val funcNameImpl = s"${func.funcName}Impl"

    val funcName = s"${func.funcName}"

    val argsTypescript = args
      .map(ad => s"${ad.name}: " + typeToTs(ad.`type`))
      .concat(List(s"$configArgName?: $configType"))
      .mkString(", ")

    val funcTypeArg = s"(${argsTypescript})"
    val funcTypeRes = s"Promise<$retTypeTs>"

    s"""
       | const ${funcNameImpl} = (peer: FluencePeer) => {
       |     return async  ${funcTypeArg}: ${funcTypeRes} => {
       |        let request: RequestFlow;
       |        const promise = new Promise<$retTypeTs>((resolve, reject) => {
       |            const r = new RequestFlowBuilder()
       |                .disableInjections()
       |                .withRawScript(
       |                    `
       |    ${tsAir.show}
       |                `,
       |                )
       |                .configHandler((h) => {
       |                    ${conf.relayVarName.fold("") { r =>
      s"""h.on('${conf.getDataService}', '$r', () => {
       |                        return peer.connectionInfo.connectedRelays[0];
       |                    });""".stripMargin
    }}    
       |                    $setCallbacks
       |                    $returnCallback
       |                    h.onEvent('${conf.errorHandlingService}', '${conf.errorFuncName}', (args) => {
       |                        const [err] = args;
       |                        reject(err);
       |                    });
       |                })
       |                .handleScriptError(reject)
       |                .handleTimeout(() => {
       |                    reject('Request timed out for ${func.funcName}');
       |                })
       |            if(${configArgName} && ${configArgName}.ttl) {
       |                r.withTTL(${configArgName}.ttl)
       |            }
       |            request = r.build();
       |        });
       |        await peer.initiateFlow(request!);
       |        return ${returnVal};
       |    }
       |}
       |
       | export const ${funcName} = ${funcNameImpl}(FluencePeer.default);
       | 
       | declare module "@fluencelabs/fluence" {
       |     interface FluencePeer {
       |         ${funcName}: ${funcTypeArg} => ${funcTypeRes}
       |     }
       | }
       |
       |FluencePeer.prototype.${funcName} = function (...args) {
       |    return ${funcNameImpl}(this)(...args);
       | };
      """.stripMargin
  }

}
