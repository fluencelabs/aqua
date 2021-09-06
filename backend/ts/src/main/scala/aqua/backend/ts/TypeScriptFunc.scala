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
    s"""h.onEvent('$callbackServiceId', '$respFuncName', async (args) => {
       |  $respBody
       |});
       |""".stripMargin

  def generate: String = {

    val tsAir = FuncAirGen(func).generate

    val retTypeTs = func.returnType
      .fold("void")(typeToTs)

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

    val configType = "{ttl?: number}"

    val funcName = s"${func.funcName}"

    val argsTypescript = args
      .map(arg => s"${fixupArgName(arg.name)}: " + typeToTs(arg.`type`))
      .concat(List(s"config?: $configType"))
    
    // defines different types for overloaded service registration function.
    var funcTypeOverload1 = argsTypescript.mkString(", ")
    var funcTypeOverload2 = ("peer: FluencePeer" :: argsTypescript).mkString(", ")

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

    val funcTypeRes = s"Promise<$retTypeTs>"

    s"""
       | export async function ${func.funcName}(${funcTypeOverload1}) : ${funcTypeRes};
       | export async function ${func.funcName}(${funcTypeOverload2}) : ${funcTypeRes};
       | export async function ${func.funcName}(...args) {
       |     let peer: FluencePeer;
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
       |     let request: RequestFlow;
       |     const promise = new Promise<$retTypeTs>((resolve, reject) => {
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
       |    await peer.internals.initiateFlow(request!);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}
