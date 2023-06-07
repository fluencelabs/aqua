package aqua.api.types

import aqua.api.*
import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.|
import scala.scalajs.js.JSConverters.*

@JSExportTopLevel("AquaFunction")
case class AquaFunction(
  @JSExport
  funcDef: FunctionDefJs,
  @JSExport
  script: String
)

@JSExportTopLevel("Input")
class Input(
  @JSExport
  val input: String
)

@JSExportTopLevel("Path")
class Path(
  @JSExport
  val path: String
)

@JSExportTopLevel("Call")
class Call(
  @JSExport
  val functionCall: String,
  @JSExport
  val arguments: js.Dynamic,
  @JSExport
  val input: Path | Input
)

@JSExportTopLevel("AquaConfig")
class AquaConfig(
  @JSExport
  val logLevel: js.UndefOr[String],
  @JSExport
  val constants: js.UndefOr[js.Array[String]],
  @JSExport
  val noXor: js.UndefOr[Boolean],
  @JSExport
  val noRelay: js.UndefOr[Boolean],
  @JSExport
  val tracing: js.UndefOr[Boolean]
)

@JSExportTopLevel("CompilationResult")
class CompilationResult(
  @JSExport
  val services: js.Dictionary[ServiceDefJs],
  @JSExport
  val functions: js.Dictionary[AquaFunction],
  @JSExport
  val functionCall: js.UndefOr[AquaFunction],
  @JSExport
  val errors: js.Array[String]
)

object CompilationResult {

  def result(
    services: js.Dictionary[ServiceDefJs],
    functions: js.Dictionary[AquaFunction],
    call: Option[AquaFunction]
  ): CompilationResult =
    new CompilationResult(services, functions, call.orNull, js.Array())

  def errs(
    errors: List[String]
  ): CompilationResult =
    CompilationResult(js.Dictionary(), js.Dictionary(), null, errors.toJSArray)
}

case class AquaAPIConfig(
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false,
  tracing: Boolean = false
) {

  def getTransformConfig: TransformConfig = {
    val transform = TransformConfig(
      wrapWithXor = !noXor,
      tracing = Option.when(tracing)(TransformConfig.TracingConfig.default)
    )

    if (noRelay) transform.copy(relayVarName = None)
    else transform
  }
}

object AquaAPIConfig {

  def fromJS(cjs: AquaConfig): AquaAPIConfig = {
    AquaAPIConfig(
      logLevel = cjs.logLevel.getOrElse("info"),
      constants = cjs.constants.map(_.toList).getOrElse(Nil),
      noXor = cjs.noXor.getOrElse(false),
      noRelay = cjs.noRelay.getOrElse(false),
      tracing = cjs.tracing.getOrElse(false)
    )
  }
}
