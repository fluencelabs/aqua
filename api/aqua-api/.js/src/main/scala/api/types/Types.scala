package api.types

import aqua.api.{AirType, AquaAPIConfig, JavaScriptType, TypeScriptType}
import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.data.Validated.{Invalid, Valid, invalidNec, validNec}

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
  val targetType: js.UndefOr[String]
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
  val source: js.UndefOr[String],
  @JSExport
  val errors: js.Array[String]
)

object CompilationResult {

  def result(
    services: js.Dictionary[ServiceDefJs],
    functions: js.Dictionary[AquaFunction],
    call: Option[AquaFunction],
    source: Option[String]
  ): CompilationResult =
    new CompilationResult(services, functions, call.orNull, source.orNull, js.Array())

  def errs(
    errors: List[String]
  ): CompilationResult =
    CompilationResult(js.Dictionary(), js.Dictionary(), null, null, errors.toJSArray)
}

object AquaConfig {

  def fromJS(cjs: AquaConfig): ValidatedNec[String, AquaAPIConfig] = {
    cjs.targetType.toOption
      .map(_.toLowerCase())
      .map {
        case "typescript" => validNec(TypeScriptType)
        case "javascript" => validNec(JavaScriptType)
        case "air" => validNec(AirType)
        case _ => invalidNec("Target can be only 'typescript', 'javascript', or 'air'")
      }
      .getOrElse(validNec(AirType))
      .map { target =>
        AquaAPIConfig(
          target,
          cjs.logLevel.getOrElse("info"),
          cjs.constants.map(_.toList).getOrElse(Nil),
          cjs.noXor.getOrElse(false),
          cjs.noRelay.getOrElse(false)
        )
      }
  }
}
