package aqua.generator

import aqua.model.FuncModel
import aqua.semantics.{ArrayType, ArrowType, DataType, LiteralType, ProductType, ScalarType, Type}
import cats.syntax.show._

case class TypescriptFunc(func: FuncModel, tsAir: Air) {

  def typeToTs(t: Type): String = t match {
    case ArrayType(t) => typeToTs(t) + "[]"
    case pt: ProductType => s"{${pt.fields.map(typeToTs).toNel.map(kv => kv._1 + ":" + kv._2).toList.mkString(",")}"
    case st: ScalarType if ScalarType.number(st) => "number"
    case ScalarType.bool => "bool"
    case ScalarType.string => "string"
    case lt: LiteralType if lt.oneOf.exists(ScalarType.number) => "number"
    case lt: LiteralType if lt.oneOf(ScalarType.bool) => "bool"
    case lt: LiteralType if lt.oneOf(ScalarType.string) => "string"
    case _: DataType => "any"
    case at: ArrowType =>
      s"(${argsToTs(at)}) => ${at.res
        .fold("void")(typeToTs)}"
  }

  def argsToTs(at: ArrowType): String =
    at.args.map(typeToTs).zipWithIndex.map(_.swap).map(kv => "arg" + kv._1 + ": " + kv._2).mkString(", ")

  def argsCallToTs(at: ArrowType): String =
    at.args.zipWithIndex.map(_._2).map("arg" + _).mkString(", ")

  def argsTypescript: String =
    func.args.map {
      case (n, Left(t)) => s"${n}: " + typeToTs(t)
      case (n, Right(at)) => s"${n}: " + typeToTs(at)
    }.mkString(", ")

  def generateTypescript: String = {

    val returnCallback = func.ret.map { case (dv, t) =>
      s"""h.on('${func.callbackService}', '${func.respFuncName}', (args) => {
         |  const [res] = args;
         |  resolve(res);
         |});
         |""".stripMargin

    }

    val setCallbacks = func.args.map {
      case (argName, Left(t)) =>
        s"""h.on('${func.getDataService}', '$argName', () => {return $argName;});"""
      case (argName, Right(at)) =>
        s"""h.on('${func.callbackService}', '$argName', (${argsToTs(at)}) => {return $argName(${argsCallToTs(
          at
        )});});"""
    }.mkString("\n")

    s"""
       |export async function ${func.name}(client: FluenceClient, ${argsTypescript}): Promise<${func.ret
      .map(_._2)
      .fold("void")(typeToTs)}> {
       |    let request;
       |    const promise = new Promise<string>((resolve, reject) => {
       |        request = new RequestFlowBuilder()
       |            .withRawScript(
       |                `
       |${tsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                h.on('${func.getDataService}', 'relay', () => {
       |                    return client.relayPeerId;
       |                });
       |                h.on('getRelayService', 'hasReleay', () => {// Not Used
       |                    return client.relayPeerId !== undefined;
       |                });
       |                $setCallbacks
       |                ${returnCallback.getOrElse("")}
       |                h.on('nameOfServiceWhereToSendXorError', 'errorProbably', (args) => {
       |                    // assuming error is the single argument
       |                    const [err] = args;
       |                    reject(err);
       |                });
       |            })
       |            .handleTimeout(() => {
       |                reject('message for timeout');
       |            })
       |            .build();
       |    });
       |    await client.initiateFlow(request);
       |    return promise;
       |}
      """.stripMargin
  }

}
