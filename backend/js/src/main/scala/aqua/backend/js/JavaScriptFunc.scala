package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.{ArgDef, FuncCallable}
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.functor._
import cats.syntax.show._

case class JavaScriptFile(func: FuncCallable) {

  import JavaScriptFile._

  def argsJavaScript: String =
    func.args.args.map(ad => s"${ad.name}").mkString(", ")

  def generateTypescript(conf: BodyConfig = BodyConfig()): String = {

    val tsAir = FuncAirGen(func).generateClientAir(conf)

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
        val value = s"$argName(${argsCallToJs(
          at
        )})"
        val expr = at.res.fold(s"$value; return {}")(_ => s"return $value")
        s"""h.on('${conf.callbackService}', '$argName', (args) => {$expr;});"""
    }.mkString("\n")

    val returnVal =
      func.ret.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    s"""
       |export async function ${func.funcName}(client${if (func.args.isEmpty) ""
    else ", "}${argsJavaScript}) {
       |    let request: RequestFlow;
       |    const promise = new Promise((resolve, reject) => {
       |        request = new RequestFlowBuilder()
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
       |            .build();
       |    });
       |    await client.initiateFlow(request);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}

object JavaScriptFile {

  def argsToTs(at: ArrowType): String =
    at.args
      .zipWithIndex
      .map(_.swap)
      .map(kv => "arg" + kv._1)
      .mkString(", ")

  def argsCallToJs(at: ArrowType): String =
    at.args.zipWithIndex.map(_._2).map(idx => s"args[$idx]").mkString(", ")

}
