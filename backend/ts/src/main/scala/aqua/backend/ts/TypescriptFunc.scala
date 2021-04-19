package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.{ArgDef, FuncCallable}
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.show._
import cats.syntax.functor._

case class TypescriptFunc(func: FuncCallable) {

  import TypescriptFunc._

  def argsTypescript: String =
    func.args.args.map(ad => s"${ad.name}: " + typeToTs(ad.`type`)).mkString(", ")

  def generateTypescript(conf: BodyConfig = BodyConfig()): String = {

    val tsAir = FuncAirGen(func).generateClientAir(conf)

    val returnCallback = func.ret.as {
      s"""h.on('${conf.callbackService}', '${conf.respFuncName}', (args) => {
         |  const [res] = args;
         |  resolve(res);
         |});
         |""".stripMargin

    }

    val setCallbacks = func.args.args.map {
      case ArgDef.Data(argName, _) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName;});"""
      case ArgDef.Arrow(argName, at) =>
        s"""h.on('${conf.callbackService}', '$argName', (args) => {return $argName(${argsCallToTs(
          at
        )});});"""
    }.mkString("\n")

    val retType = func.ret
      .map(_.`type`)
      .fold("void")(typeToTs)

    val returnVal =
      func.ret.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    s"""
       |export async function ${func.funcName}(client: FluenceClient${if (func.args.isEmpty) ""
    else ", "}${argsTypescript}): Promise<$retType> {
       |    let request;
       |    const promise = new Promise<$retType>((resolve, reject) => {
       |        request = new RequestFlowBuilder()
       |            .withRawScript(
       |                `
       |${tsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                h.on('${conf.getDataService}', 'relay', () => {
       |                    return client.relayPeerId;
       |                });
       |                h.on('getRelayService', 'hasReleay', () => {// Not Used
       |                    return client.relayPeerId !== undefined;
       |                });
       |                $setCallbacks
       |                ${returnCallback.getOrElse("")}
       |                h.on('${conf.errorHandlingService}', '${conf.errorFuncName}', (args) => {
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

object TypescriptFunc {

  def typeToTs(t: Type): String = t match {
    case ArrayType(t) => typeToTs(t) + "[]"
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
