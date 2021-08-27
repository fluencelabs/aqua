package aqua.backend.js

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

case class JavaScriptFunc(func: FuncRes) {

  import JavaScriptCommon._
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

          s""" let opt = args;
            |$unwrapOpts
            | return resolve(opt);""".stripMargin
        case _ =>
          """  const [res] = args;
            |  resolve(res);""".stripMargin
        }
      case None => ""
    }
    s"""h.onEvent('$callbackServiceId', '$respFuncName', async (args) => {
       |  $respBody
       |});
       |""".stripMargin

  def generate: String = {

    val tsAir = FuncAirGen(func).generate

    val setCallbacks = func.args.collect { // Product types are not handled
      case Arg(argName, OptionType(_)) =>
        s"""h.on('$dataServiceId', '$argName', async () => {return ${fixupArgName(argName)} === null ? [] : [${fixupArgName(argName)}];});"""
      case Arg(argName, _: DataType) =>
        s"""h.on('$dataServiceId', '$argName', async () => {return ${fixupArgName(argName)};});"""
      case Arg(argName, at: ArrowType) =>
        s"""
           | h.use(async (req, resp, next) => {
           | if(req.serviceId === '${conf.callbackService}' && req.fnName === '$argName') {
           |     ${callBackExprBody(at, argName)}
           | }
           | await next();
           | });
        """.stripMargin
    }
      .mkString("\n")

    val returnVal =
      func.returnType.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val clientArgName = genArgName("client")
    val configArgName = genArgName("config")

    val funcName = s"${func.funcName}"

    val argsLets = args.map(arg => s"let ${fixupArgName(arg.name)};").mkString("\n")

    val argsFormAssn = args
      .map(arg => fixupArgName(arg.name))
      .concat(List("config"))
      .zipWithIndex

    // argument upnacking has two forms. 
    // One starting from the first (by index) argument,
    // One starting from zero
    var argsAssignmentStartingFrom1 = argsFormAssn.map((name, ix) => s"${name} = args[${ix + 1}];").mkString("\n")
    var argsAssignmentStartingFrom0 = argsFormAssn.map((name, ix) => s"${name} = args[${ix}];").mkString("\n")

    s"""
       | export async function ${func.funcName}(...args) {
       |     let peer;
       |     ${argsLets}
       |     let config;
       |     if (args[0] instanceof FluencePeer) {
       |         peer = args[0];
       |         ${argsAssignmentStartingFrom1}
       |     } else {
       |         peer = FluencePeer.default;
       |         ${argsAssignmentStartingFrom0}
       |     }
       |    
       |     let request;
       |     const promise = new Promise((resolve, reject) => {
       |         const r = new RequestFlowBuilder()
       |                 .disableInjections()
       |                 .withRawScript(
       |                     `
       |     ${tsAir.show}
       |                 `,
       |                 )
       |                 .configHandler((h) => {
       |                     ${conf.relayVarName.fold("") { r =>
      s"""h.on('${conf.getDataService}', '$r', async () => {
       |                    return peer.connectionInfo.connectedRelays[0] || null;
       |                });""".stripMargin  }}
       |                $setCallbacks
       |                $returnCallback
       |                h.onEvent('${conf.errorHandlingService}', '${conf.errorFuncName}', async (args) => {
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
       |    await peer.initiateFlow(request);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}
