package aqua.model

import aqua.generator.DataView.{InitPeerId, StringScalar}
import aqua.generator.{Air, AirContext, AirGen, ArrowCallable, DataView, FuncCallable, SrvCallableOnPeer}
import aqua.semantics.{ArrayType, ArrowType, DataType, Type}
import cats.data.{Chain, NonEmptyChain}
import cats.syntax.show._

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(DataView, Type)],
  body: FuncOp
) extends Model {

  def bodyGen: AirGen = body.toAirGen

  def getDataService: String = "getDataSrv"
  def callbackService: String = "callbackSrv"

  def callable: ArrowCallable =
    new FuncCallable(args.map(_._1), ret.map(_._1), bodyGen)

  def typeToTs(t: Type): String = t match {
    case ArrayType(t) => typeToTs(t) + "[]"
    case dt: DataType => "any" // TODO render types
    case at: ArrowType =>
      s"(${argsToTs(at)}) => ${at.res
        .fold("void")(_ => "any")}"
  }

  def argsToTs(at: ArrowType): String =
    at.args.map(typeToTs).zipWithIndex.map(_.swap).map(kv => "arg" + kv._1 + ": " + kv._2).mkString(", ")

  def argsCallToTs(at: ArrowType): String =
    at.args.zipWithIndex.map(_._2).map("arg" + _).mkString(", ")

  def argsTypescript: String =
    args.map {
      case (n, Left(t)) => s"${n}: " + typeToTs(t)
      case (n, Right(at)) => s"${n}: " + typeToTs(at)
    }.mkString(", ")

  def airContext(acc: Map[String, ArrowCallable]): AirContext =
    AirContext(
      data = args.collect { //TODO preload these variables
        case (an, Left(_)) =>
          an -> DataView.Variable(an)
      }.toMap,
      arrows = acc ++ args.collect { case (an, Right(_)) =>
        an -> new SrvCallableOnPeer(InitPeerId, DataView.StringScalar(callbackService), an)
      }.toMap,
      vars = args.map(_._1).toSet
    )

  def generateAir(acc: Map[String, ArrowCallable]): Air =
    bodyGen
      .generate(airContext(acc))
      ._2

  def generateTypescript(acc: Map[String, ArrowCallable]): String = {
    def getDataOp(name: String): FuncOp =
      CoalgebraModel(
        Some(ServiceModel(getDataService, StringScalar("\"" + getDataService + "\""))),
        name,
        Nil,
        Some(name)
      )

    val air = SeqModel(
      NonEmptyChain.fromChainAppend(
        Chain.fromSeq(
          args.collect { case (argName, Left(_)) =>
            getDataOp(name)
          }
        ),
        body
      )
    ).toAirGen.generate(airContext(acc))._2.show

    val setCallbacks = args.map {
      case (argName, Left(t)) =>
        s"""h.on('$getDataService', '$argName', () => {return $argName;});"""
      case (argName, Right(at)) =>
        s"""h.on('$callbackService', '$argName', (${argsToTs(at)}) => {return $argName(${argsCallToTs(at)});});"""
    }.mkString("\n")

    s"""
       |export async function ${name}(client: FluenceClient, ${argsTypescript}): Promise<${ret
      .map(_._2)
      .fold("void")(typeToTs)}> {
       |    let request;
       |    const promise = new Promise<string>((resolve, reject) => {
       |        request = new RequestFlowBuilder()
       |            .withRawScript(
       |                `
       |$air
       |            `,
       |            )
       |            .configHandler((h) => {
       |                h.on('getRelayService', 'getRelay', () => {
       |                    return client.relayPeerId;
       |                });
       |                h.on('getRelayService', 'hasReleay', () => {
       |                    return client.relayPeerId !== undefined;
       |                });
       |                $setCallbacks
       |                h.on('nameForServiceWhichResolvesPromise', 'callbackOrAnythingReally', (args) => {
       |                    // args is an array of all the arguments to function.
       |                    // Extract the right one from the args. If there is only 1 item, you can always use
       |                    // the costruct below
       |                    const [res] = args;
       |                    resolve(res);
       |                });
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
