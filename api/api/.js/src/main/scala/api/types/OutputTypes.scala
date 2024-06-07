package api.types

import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.transform.TransformConfig
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.|

class AquaFunction(
  val funcDef: FunctionDefJs,
  val script: String
) extends js.Object

class GeneratedSource(
  val name: String,
  val tsSource: js.UndefOr[String],
  val jsSource: js.UndefOr[String],
  val tsTypes: js.UndefOr[String]
) extends js.Object

object GeneratedSource {

  def tsSource(name: String, tsSource: String) =
    new GeneratedSource(name, tsSource, null, null)

  def jsSource(name: String, jsSource: String, tsTypes: String) =
    new GeneratedSource(name, null, jsSource, tsTypes)
}

class CompilationResult(
  val services: js.Dictionary[ServiceDefJs],
  val functions: js.Dictionary[AquaFunction],
  val functionCall: js.UndefOr[AquaFunction],
  val generatedSources: js.Array[GeneratedSource],
  val errors: js.Array[String],
  val warnings: js.Array[String]
) extends js.Object

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
