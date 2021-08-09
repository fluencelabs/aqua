package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.func.FuncCallable
import aqua.model.transform.GenerationConfig
import aqua.types._
import cats.syntax.show._

case class TypeScriptFunc(func: FuncCallable) {

  import TypeScriptFunc._

  def argsTypescript: String =
    func.arrowType.domain.toLabelledList().map(ad => s"${ad._1}: " + typeToTs(ad._2)).mkString(", ")

  def generateUniqueArgName(args: List[String], basis: String, attempt: Int): String = {
    val name = if (attempt == 0) {
      basis
    } else {
      basis + attempt
    }
    args.find(_ == name).map(_ => generateUniqueArgName(args, basis, attempt + 1)).getOrElse(name)
  }

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
    s"""h.onEvent('$callbackService', '$respFuncName', (args) => {
       |  $body
       |});
       |""".stripMargin
  }

  def generateTypescript(conf: GenerationConfig = GenerationConfig()): String = {

    val tsAir = FuncAirGen(func).generateAir(conf)

    // TODO: support multi return
    val retType =
      if (func.arrowType.codomain.length > 1) Some(func.arrowType.codomain)
      else func.arrowType.codomain.uncons.map(_._1)
    val retTypeTs = retType
      .fold("void")(typeToTs)

    val returnCallback = retType
      .map(t => genReturnCallback(t, conf.callbackService, conf.respFuncName))
      .getOrElse("")

    val setCallbacks = func.args.collect { // Product types are not handled
      case (argName, OptionType(_)) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName === null ? [] : [$argName];});"""
      case (argName, _: DataType) =>
        s"""h.on('${conf.getDataService}', '$argName', () => {return $argName;});"""
      case (argName, at: ArrowType) =>
        val value = s"$argName(${argsCallToTs(
          at
        )})"
        val expr = at.res.fold(s"$value; return {}")(_ => s"return $value")
        s"""h.on('${conf.callbackService}', '$argName', (args) => {$expr;});"""
    }
      .mkString("\n")

    // TODO support multi return
    val returnVal =
      func.ret.headOption.fold("Promise.race([promise, Promise.resolve()])")(_ => "promise")

    val clientArgName = generateUniqueArgName(func.argNames, "client", 0)
    val configArgName = generateUniqueArgName(func.argNames, "config", 0)

    val configType = "{ttl?: number}"

    s"""
       |export async function ${func.funcName}($clientArgName: FluenceClient${if (func.args.isEmpty)
      ""
    else ", "}${argsTypescript}, $configArgName?: $configType): Promise<$retTypeTs> {
       |    let request: RequestFlow;
       |    const promise = new Promise<$retTypeTs>((resolve, reject) => {
       |        const r = new RequestFlowBuilder()
       |            .disableInjections()
       |            .withRawScript(
       |                `
       |${tsAir.show}
       |            `,
       |            )
       |            .configHandler((h) => {
       |                ${conf.relayVarName.fold("") { r =>
      s"""h.on('${conf.getDataService}', '$r', () => {
       |                    return $clientArgName.relayPeerId!;
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
       |        if(${configArgName} && ${configArgName}.ttl) {
       |            r.withTTL(${configArgName}.ttl)
       |        }
       |        request = r.build();
       |    });
       |    await $clientArgName.initiateFlow(request!);
       |    return ${returnVal};
       |}
      """.stripMargin
  }

}

object TypeScriptFunc {

  def typeToTs(t: Type): String = t match {
    case OptionType(t) => typeToTs(t) + " | null"
    case ArrayType(t) => typeToTs(t) + "[]"
    case StreamType(t) => typeToTs(t) + "[]"
    case pt: ProductType =>
      "[" + pt.toList.map(typeToTs).mkString(", ") + "]"
    case st: StructType =>
      s"{${st.fields.map(typeToTs).toNel.map(kv => kv._1 + ":" + kv._2).toList.mkString(";")}}"
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
    case _ =>
      // TODO: handle product types in returns
      "any"
  }

  def argsToTs(at: ArrowType): String =
    at.domain
      .toLabelledList()
      .map(nt => nt._1 + ": " + typeToTs(nt._2))
      .mkString(", ")

  def argsCallToTs(at: ArrowType): String =
    at.domain.toList.zipWithIndex.map(_._2).map(idx => s"args[$idx]").mkString(", ")

}
