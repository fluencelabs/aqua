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
  val noRelay: js.UndefOr[Boolean]
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
  noRelay: Boolean = false
) {

  def getTransformConfig: TransformConfig =
    if (noRelay) TransformConfig(relayVarName = None, wrapWithXor = !noXor)
    else TransformConfig(wrapWithXor = !noXor)
}

object AquaAPIConfig {

  def fromJS(cjs: AquaConfig): AquaAPIConfig = {
    AquaAPIConfig(
      cjs.logLevel.getOrElse("info"),
      cjs.constants.map(_.toList).getOrElse(Nil),
      cjs.noXor.getOrElse(false),
      cjs.noRelay.getOrElse(false)
    )
  }
}
