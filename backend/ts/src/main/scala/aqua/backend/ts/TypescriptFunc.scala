package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.FuncResolved
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.show._

case class TypescriptFunc(func: FuncResolved) {

  import TypescriptFunc._

  def argsTypescript: String =
    func.func.args.map {
      case (n, Left(t)) => s"${n}: " + typeToTs(t)
      case (n, Right(at)) => s"${n}: " + typeToTs(at)
    }.mkString(", ")

  def generateTypescript(conf: BodyConfig = BodyConfig()): String = {

    val tsAir = FuncAirGen(func).generateClientAir(conf)

    val returnCallback = func.func.ret.map { case (dv, t) =>
      s"""h.on('${conf.callbackService}', '${conf.respFuncName}', (args) => {
         |  const [res] = args;
         |  resolve(res);
         |});
         |""".stripMargin

    }

    val setCallbacks = func.func.args.map {
      case (argName, Left(t)) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName;});"""
      case (argName, Right(at)) =>
        s"""h.on('${conf.callbackService}', '$argName', (args) => {return $argName(${argsCallToTs(
          at
        )});});"""
    }.mkString("\n")

    val retType = func.func.ret
      .map(_._2)
      .fold("void")(typeToTs)

    s"""
       |export async function ${func.name}(client: FluenceClient${if (func.func.args.isEmpty) ""
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
       |                h.on('${conf.errorHandlingService}', '${conf.error}', (args) => {
       |                    // assuming error is the single argument
       |                    const [err] = args;
       |                    reject(err);
       |                });
       |            })
       |            .handleScriptError(reject)
       |            .handleTimeout(() => {
       |                reject('Request timed out');
       |            })
       |            .build();
       |    });
       |    await client.initiateFlow(request);
       |    return promise;
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
