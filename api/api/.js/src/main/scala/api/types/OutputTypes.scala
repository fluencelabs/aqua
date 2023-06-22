package api.types

import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig
import cats.data.Validated.{Invalid, Valid, invalidNec, validNec}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.|

@JSExportTopLevel("AquaFunction")
case class AquaFunction(
  @JSExport
  funcDef: FunctionDefJs,
  @JSExport
  script: String
)

@JSExportTopLevel("GeneratedSource")
case class GeneratedSource(
  @JSExport
  val name: String,
  @JSExport
  val tsSource: js.UndefOr[String],
  @JSExport
  val jsSource: js.UndefOr[String],
  @JSExport
  val tsTypes: js.UndefOr[String]
)

object GeneratedSource {
  def tsSource(name: String, tsSource: String) = new GeneratedSource(name, tsSource, null, null)
  def jsSource(name: String, jsSource: String, tsTypes: String) = new GeneratedSource(name, null, jsSource, tsTypes)
}

@JSExportTopLevel("CompilationResult")
class CompilationResult(
  @JSExport
  val services: js.Dictionary[ServiceDefJs],
  @JSExport
  val functions: js.Dictionary[AquaFunction],
  @JSExport
  val functionCall: js.UndefOr[AquaFunction],
  @JSExport
  val generatedSources: js.Array[GeneratedSource],
  @JSExport
  val errors: js.Array[String]
)

object CompilationResult {

  def result(
    services: js.Dictionary[ServiceDefJs] = js.Dictionary(),
    functions: js.Dictionary[AquaFunction] = js.Dictionary(),
    call: Option[AquaFunction] = None,
    sources: js.Array[GeneratedSource] = js.Array()
  ): CompilationResult =
    new CompilationResult(services, functions, call.orNull, sources, js.Array())

  def errs(
    errors: List[String]
  ): CompilationResult =
    CompilationResult(js.Dictionary(), js.Dictionary(), null, null, errors.toJSArray)
}
