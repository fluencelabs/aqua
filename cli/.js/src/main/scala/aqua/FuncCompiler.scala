package aqua

import aqua.ErrorRendering.showError
import aqua.compiler.{AquaCompiler, AquaCompilerConf, CompilerAPI}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.json.JsonEncoder
import aqua.model.transform.TransformConfig
import aqua.model.{AquaContext, FuncArrow, ServiceModel}
import aqua.parser.lift.FileSpan
import aqua.raw.ConstantRaw
import aqua.run.RunCommand.logger
import aqua.run.{JsonService, Runner}
import aqua.types.{ArrowType, NilType, ProductType}
import cats.data.Validated.{invalidNec, validNec}
import cats.data.{Chain, NonEmptyList, Validated, ValidatedNec}
import cats.effect.IO
import cats.effect.kernel.{Async, Clock}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.scalajs.js

// Function compiler
class FuncCompiler[F[_]: Files: AquaIO: Async](
  input: Path,
  imports: List[Path],
  transformConfig: TransformConfig,
  withRunImport: Boolean = false
) extends Logging {

  private def findFunction(
    contexts: Chain[AquaContext],
    func: CliFunc,
    services: List[JsonService]
  ): ValidatedNec[String, (FuncArrow, List[TypedJsonService])] = {
    contexts
      .collectFirstSome(c => c.allFuncs.get(func.name).map(f => (f, c)))
      .map(validNec)
      .getOrElse(
        Validated.invalidNec[String, FuncArrow](
          s"There is no function '${func.name}' or it is not exported. Check the spelling or see https://doc.fluence.dev/aqua-book/language/header#export"
        )
      )
      .andThen { case (func, context) =>
        findServices(context, services).map { l =>
          println(l)
          (func, l)
        }
      }
  }

  case class TypedJsonFunction(name: String, result: js.Dynamic, resultType: ProductType)
  case class TypedJsonService(name: String, functions: NonEmptyList[TypedJsonFunction])

  private def findServices(
    context: AquaContext,
    services: List[JsonService]
  ): ValidatedNec[String, List[TypedJsonService]] = {
    println(context.services.keys)
    println(context.abilities.keys)
    println(context.funcs.keys)
    services
      .map(js =>
        context.services
          .get(js.name)
          .map(sm => (js, sm))
          .map(validNec)
          .getOrElse(
            Validated.invalidNec[String, ServiceModel](
              s"There is no service '${js.name}' (described in json-service file) in aqua source or it is not exported. Check the spelling or see https://doc.fluence.dev/aqua-book/language/header#export"
            )
          )
      )
      .sequence
      .andThen { l =>
        l.map { case (js: JsonService, sm: ServiceModel) =>
          val typedFunctions: ValidatedNec[String, NonEmptyList[TypedJsonFunction]] =
            js.functions.map { jf =>
              sm.arrows(jf.name)
                .map { case arr: ArrowType =>
                  if (arr.domain.isEmpty)
                    Runner
                      .validateTypes(jf.name, arr.codomain, Some(ProductType(jf.resultType :: Nil)))
                      .map(_ => TypedJsonFunction(jf.name, jf.result, arr.codomain))
                  else
                    invalidNec(s"Json service '${jf.name}' cannot have any arguments")
                }
                .getOrElse(
                  Validated.invalidNec[String, TypedJsonFunction](
                    s"There is no function '${jf.name}' in service '${js.name}' in aqua source. Check your 'json-service' options"
                  )
                )
            }.sequence

          typedFunctions.map(tfs => TypedJsonService(js.name, tfs))
        }.sequence
      }
  }

  // Compile and get only one function
  def compile(
    func: CliFunc,
    jsonServices: List[JsonService]
  ): F[ValidatedNec[String, FuncArrow]] = {
    for {
      prelude <- Prelude.init[F](withRunImport)
      sources = new AquaFileSources[F](input, prelude.importPaths ++ imports)
      // compile only context to wrap and call function later
      compileResult <- Clock[F].timed(
        CompilerAPI
          .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            AquaCompilerConf(transformConfig.constantsList)
          )
          .map(_.leftMap(_.map(_.show)))
      )
      (compileTime, contextV) = compileResult
    } yield {
      logger.debug(s"Compile time: ${compileTime.toMillis}ms")
      contextV.andThen(c => findFunction(c, func, jsonServices).map(_._1))
    }
  }
}
