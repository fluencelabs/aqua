package aqua.backend.js

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*

case class JavaScriptFunc(func: FuncRes) {

  import JavaScriptFunc._
  import FuncRes._
  import func._

  def argsJavaScript: String =
    argNames.mkString(", ")

  // TODO: use common functions between TypeScript and JavaScript backends
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

        s""" let opt = args;
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

    val jsAir = FuncAirGen(func).generate

    val setCallbacks = func.args.collect {
      case Arg(argName, OptionType(_)) =>
        s"""h.on('$dataServiceId', '$argName', () => {return $argName === null ? [] : [$argName];});"""
      case Arg(argName, _: DataType) =>
        s"""h.on('$dataServiceId', '$argName', () => {return $argName;});"""
      case Arg(argName, at: ArrowType) =>
        val value = s"$argName(${argsCallToJs(
          at
        )})"
        val expr = arrowToRes(at).fold(s"$value; return {}")(_ => s"return $value")
        s"""h.on('$callbackServiceId', '$argName', (args) => {$expr;});"""
    }
      .mkString("\n")

    val returnVal =
      returnType.headOption.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val clientArgName = genArgName("client")
    val configArgName = genArgName("config")

    s"""
       |export async function ${func.funcName}(${clientArgName}${if (func.args.isEmpty) ""
    else ", "}${argsJavaScript}, $configArgName) {
       |    let request;
       |    $configArgName = $configArgName || {};
       |    const promise = new Promise((resolve, reject) => {
       |        var r = new RequestFlowBuilder()
       |            .disableInjections()
       |            .withRawScript(
       |                `
       |${jsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                ${relayVarName.fold("") { r =>
      s"""h.on('$dataServiceId', '$r', () => {
       |                    return client.relayPeerId;
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
       |        if(${configArgName}.ttl) {
       |            r.withTTL(${configArgName}.ttl)
       |        }
       |        request = r.build();
       |    });
       |    await ${clientArgName}.initiateFlow(request);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}

object JavaScriptFunc {

  def argsCallToJs(at: ArrowType): String =
    FuncRes.arrowArgIndices(at).map(idx => s"args[$idx]").mkString(", ")

}
