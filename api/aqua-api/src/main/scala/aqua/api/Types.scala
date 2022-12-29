package aqua.api

import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("AquaFunction")
case class AquaFunction(
  @JSExport
  funcDef: FunctionDefJs,
  @JSExport
  script: String
)

@JSExportTopLevel("AquaConfig")
case class AquaConfig(
  @JSExport
  logLevel: js.UndefOr[String],
  @JSExport
  constants: js.UndefOr[js.Array[String]],
  @JSExport
  noXor: js.UndefOr[Boolean],
  @JSExport
  noRelay: js.UndefOr[Boolean]
)

@JSExportTopLevel("CompilationRunResult")
case class CompilationRunResult(
  @JSExport
  aquaFunction: js.UndefOr[AquaFunction],
  @JSExport
  errors: js.Array[String]
)

@JSExportTopLevel("CompilationResult")
case class CompilationResult(
  @JSExport
  services: js.Dictionary[ServiceDefJs],
  @JSExport
  functions: js.Dictionary[AquaFunction],
  @JSExport
  errors: js.Array[String]
)

case class AquaAPIConfig(
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false
) {

  def getTransformConfig: TransformConfig =
    if (noRelay) TransformConfig(wrapWithXor = !noXor)
    else TransformConfig(relayVarName = None, wrapWithXor = !noXor)
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

object CompilationResult {

  def result(
    services: js.Dictionary[ServiceDefJs],
    functions: js.Dictionary[AquaFunction]
  ): CompilationResult =
    CompilationResult(services, functions, js.Array())

  def errs(
    errors: js.Array[String]
  ): CompilationResult =
    CompilationResult(js.Dictionary(), js.Dictionary(), errors)
}
