package api.types

import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig

import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
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

  def tsSource(name: String, tsSource: String) =
    new GeneratedSource(name, tsSource, null, null)

  def jsSource(name: String, jsSource: String, tsTypes: String) =
    new GeneratedSource(name, null, jsSource, tsTypes)
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
  val errors: js.Array[String],
  @JSExport
  val warnings: js.Array[String]
)

object CompilationResult {

  def result(
    services: Map[String, ServiceDefJs] = Map.empty,
    functions: Map[String, AquaFunction] = Map.empty,
    call: Option[AquaFunction] = None,
    sources: List[GeneratedSource] = List.empty,
    warnings: List[String] = List.empty
  ): CompilationResult =
    new CompilationResult(
      services.toJSDictionary,
      functions.toJSDictionary,
      call.orNull,
      sources.toJSArray,
      js.Array(),
      warnings.toJSArray
    )

  def errs(
    errors: List[String] = List.empty,
    warnings: List[String] = List.empty
  ): CompilationResult =
    new CompilationResult(
      js.Dictionary.empty,
      js.Dictionary.empty,
      null,
      null,
      errors.toJSArray,
      warnings.toJSArray
    )
}
