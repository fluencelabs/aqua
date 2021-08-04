package aqua.backend.js

import aqua.backend.air.FuncAirGen
import aqua.model.func.FuncCallable
import aqua.model.transform.GenerationConfig
import aqua.types._
import cats.syntax.show._

case class JavaScriptFunc(func: FuncCallable) {

  import JavaScriptFunc._

  def argsJavaScript: String =
    func.args.toLabelledList().map(ad => s"${ad._1}").mkString(", ")

  // TODO: use common functions between TypeScript and JavaScript backends
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

  def generateJavascript(conf: GenerationConfig = GenerationConfig()): String = {

    val tsAir = FuncAirGen(func).generateAir(conf)

    val setCallbacks = func.args
      .toLabelledList()
      .collect {
        case (argName, OptionType(_)) =>
          s"""h.on('${conf.getDataService}', '$argName', () => {return $argName === null ? [] : [$argName];});"""
        case (argName, _: DataType) =>
          s"""h.on('${conf.getDataService}', '$argName', () => {return $argName;});"""
        case (argName, at: ArrowType) =>
          val value = s"$argName(${argsCallToJs(
            at
          )})"
          val expr = at.res.fold(s"$value; return {}")(_ => s"return $value")
          s"""h.on('${conf.callbackService}', '$argName', (args) => {$expr;});"""
      }
      .mkString("\n")

    val returnCallback = func.ret
      .map(_.lastType)
      .map(t => genReturnCallback(t, conf.callbackService, conf.respFuncName))
      .getOrElse("")

    val returnVal =
      func.ret.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    // TODO: it could be non-unique
    val configArgName = "config"

    s"""
       |export async function ${func.funcName}(client${if (func.args.isEmpty) ""
    else ", "}${argsJavaScript}, $configArgName) {
       |    let request;
       |    $configArgName = $configArgName || {};
       |    const promise = new Promise((resolve, reject) => {
       |        var r = new RequestFlowBuilder()
       |            .disableInjections()
       |            .withRawScript(
       |                `
       |${tsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                ${conf.relayVarName.fold("") { r =>
      s"""h.on('${conf.getDataService}', '$r', () => {
       |                    return client.relayPeerId;
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
       |        if(${configArgName}.ttl) {
       |            r.withTTL(${configArgName}.ttl)
       |        }
       |        request = r.build();
       |    });
       |    await client.initiateFlow(request);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}

object JavaScriptFunc {

  def argsToTs(at: ArrowType): String =
    at.args.zipWithIndex
      .map(_.swap)
      .map(kv => "arg" + kv._1)
      .mkString(", ")

  def argsCallToJs(at: ArrowType): String =
    at.args.zipWithIndex.map(_._2).map(idx => s"args[$idx]").mkString(", ")

}
