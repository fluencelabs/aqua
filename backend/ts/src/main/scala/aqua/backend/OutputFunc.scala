package aqua.backend

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypeScriptCommon.{callBackExprBody, fixupArgName}
import aqua.backend.ts.{TSFuncTypes, TypeScriptCommon}
import aqua.model.transform.res.FuncRes
import aqua.model.transform.res.FuncRes.Arg
import aqua.types.{ArrowType, DataType, OptionType, ProductType}
import cats.syntax.show.*

case class OutputFunc(func: FuncRes, types: Types) {

  import FuncRes.*
  import TypeScriptCommon.*
  import types.*
  import func.*
  val funcTypes = types.funcType(func)
  import funcTypes.*

  val argsFormAssingment = args
    .map(arg => fixupArgName(arg.name))
    .appended("config")
    .zipWithIndex

  private def returnCallback: String =
    val respBody = func.returnType match {
      case Some(x) => x match {
        case OptionType(_) =>
          """                        let [opt] = args;
            |                        if (Array.isArray(opt)) {
            |                            if (opt.length === 0) { resolve(null); }
            |                            opt = opt[0];
            |                        }
            |                        return resolve(opt);""".stripMargin
        case pt: ProductType =>
          val unwrapOpts = pt.toList.zipWithIndex.collect { case (OptionType(_), i) =>
            s"""                        if( Array.isArray(opt[$i])) {
               |                            if (opt[$i].length === 0) { opt[$i] = null; }
               |                            else {opt[$i] = opt[$i][0]; }
               |                        }""".stripMargin
          }.mkString

          s"""                    let ${typed("opt", "any")} = args;
             |$unwrapOpts
             |                    return resolve(opt);""".stripMargin
        case _ =>
          """                        const [res] = args;
            |                        resolve(res);""".stripMargin
        }
      case None => ""
    }
    s"""                    h.onEvent('$callbackServiceId', '$respFuncName', (args) => {
       |$respBody
       |                    });""".stripMargin

  def generate: String = {

    val tsAir = FuncAirGen(func).generate

    val setCallbacks = func.args.collect { // Product types are not handled
      case Arg(argName, OptionType(_)) =>
        s"""                    h.on('$dataServiceId', '$argName', () => {return ${fixupArgName(argName)} === null ? [] : [${fixupArgName(argName)}];});"""
      case Arg(argName, _: DataType) =>
        s"""                    h.on('$dataServiceId', '$argName', () => {return ${fixupArgName(argName)};});"""
      case Arg(argName, at: ArrowType) =>
        s"""                    h.use((req, resp, next) => {
           |                        if(req.serviceId === '${conf.callbackService}' && req.fnName === '$argName') {
           |${callBackExprBody(at, argName, 28)}
           |                        }
           |                        next();
           |                    });
        """.stripMargin
    }
      .mkString("\n")

    val returnVal =
      func.returnType.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val configArgName = genArgName("config")

    val codeLeftSpace = " " * 20

    val argsLets = args.map(arg => s"    let ${typed(fixupArgName(arg.name), "any")};").mkString("\n")

    // argument upnacking has two forms.
    // One starting from the first (by index) argument,
    // One starting from zero
    val argsAssignmentStartingFrom1 = argsFormAssingment.map((name, ix) => s"        ${name} = args[${ix + 1}];").mkString("\n")
    val argsAssignmentStartingFrom0 = argsFormAssingment.map((name, ix) => s"        ${name} = args[${ix}];").mkString("\n")

    s"""
       |${funcTypes.generate}
       |export function ${func.funcName}(${typed("...args", "any")}) {
       |    let ${typed("peer", "FluencePeer")};
       |${argsLets}
       |    let ${typed("config", "any")};
       |    if (FluencePeer.isInstance(args[0])) {
       |        peer = args[0];
       |${argsAssignmentStartingFrom1}
       |    } else {
       |        peer = Fluence.getPeer();
       |${argsAssignmentStartingFrom0}
       |    }
       |
       |    let ${typed("request", "RequestFlow")};
       |    const promise = new ${generic("Promise", retTypeTs._2)}((resolve, reject) => {
       |        const r = new RequestFlowBuilder()
       |                .disableInjections()
       |                .withRawScript(`
       |${tsAir.show.linesIterator.map(codeLeftSpace + _).mkString("\n")}
       |                `,
       |                )
       |                .configHandler((h) => {
       |                    ${conf.relayVarName.fold("") { r =>
      s"""h.on('${conf.getDataService}', '$r', () => {
       |                        return peer.getStatus().relayPeerId;
       |                    });""".stripMargin  }}
       |$setCallbacks
       |$returnCallback
       |                    h.onEvent('${conf.errorHandlingService}', '${conf.errorFuncName}', (args) => {
       |                        const [err] = args;
       |                        reject(err);
       |                    });
       |                })
       |                .handleScriptError(reject)
       |                .handleTimeout(() => {
       |                    reject('Request timed out for ${func.funcName}');
       |                })
       |
       |                if (${configArgName} && ${configArgName}.ttl) {
       |                    r.withTTL(${configArgName}.ttl)
       |                }
       |
       |                request = r.build();
       |    });
       |    peer.internals.initiateFlow(${bang("request")});
       |    return ${returnVal};
       |}""".stripMargin
  }

}
