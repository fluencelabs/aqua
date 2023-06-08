package api.types

import aqua.api.AquaAPIConfig
import aqua.api.TargetType.*
import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.|

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
  val tracing: js.UndefOr[Boolean],
  @JSExport
  val targetType: js.UndefOr[String]
)

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
          targetType = target,
          logLevel = cjs.logLevel.getOrElse("info"),
          constants = cjs.constants.map(_.toList).getOrElse(Nil),
          noXor = cjs.noXor.getOrElse(false),
          noRelay = cjs.noRelay.getOrElse(false),
          tracing = cjs.tracing.getOrElse(false)
        )
      }
  }
}
