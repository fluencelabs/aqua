package aqua.api

import aqua.ErrorRendering.showError
import aqua.raw.value.ValueRaw
import aqua.api.AquaAPIConfig
import aqua.backend.{AirFunction, Backend, Generated}
import aqua.compiler.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.logging.{LogFormatter, LogLevels}
import aqua.constants.Constants
import aqua.io.*
import aqua.raw.ops.Call
import aqua.run.{CallInfo, CallPreparer, CliFunc, FuncCompiler, RunPreparer}
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.{CompilerState, HeaderError, RulesViolated, WrongAST}
import aqua.{AquaIO, SpanParser}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.backend.api.APIBackend
import aqua.definitions.FunctionDef
import aqua.model.AquaContext
import aqua.res.AquaRes
import cats.Applicative
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.data.Validated.{Invalid, Valid, invalid, invalidNec, validNec}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.show.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scribe.Logging

object APICompilation {

  def compileCall(
    functionStr: String,
    pathStr: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig,
    fillWithTypes: List[ValueRaw] => ValidatedNec[String, List[ValueRaw]]
  ): IO[ValidatedNec[String, (FunctionDef, String)]] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).mapN { (level, constants) =>

      val transformConfig = aquaConfig.getTransformConfig.copy(constants = constants)

      LogFormatter.initLogger(Some(level))

      new FuncCompiler[IO](
        Some(RelativePath(Path(pathStr))),
        imports.map(Path.apply),
        transformConfig
      ).compile().map { contextV =>
        contextV.andThen { context =>
          CliFunc
            .fromString(functionStr)
            .leftMap(errs => NonEmptyChain.fromNonEmptyList(errs))
            .andThen { cliFunc =>
              FuncCompiler.findFunction(context, cliFunc).andThen { arrow =>
                fillWithTypes(cliFunc.args).andThen { argsWithTypes =>
                  val func = cliFunc.copy(args = argsWithTypes)
                  val preparer = new RunPreparer(
                    func,
                    arrow,
                    transformConfig
                  )
                  preparer.prepare().map { ci =>
                    (ci.definitions, ci.air)
                  }
                }
              }
            }
        }.leftMap(_.map(_.show).distinct)

      }
    } match {
      case Valid(pr) => pr
      case Invalid(errs) => IO.pure(Invalid(NonEmptyChain.fromNonEmptyList(errs)))
    }
  }

  def compilePath(
    pathStr: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig,
    backend: Backend
  ): IO[ValidatedNec[String, Chain[AquaCompiled[FileModuleId]]]] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val path = Path(pathStr)
    val sources = new AquaFileSources[IO](path, imports.map(Path.apply))
    compileRaw(
      aquaConfig,
      sources,
      backend
    )
  }

  def compileString(
    input: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig,
    backend: Backend
  ): IO[ValidatedNec[String, Chain[AquaCompiled[FileModuleId]]]] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val path = Path("")

    val strSources: AquaFileSources[IO] =
      new AquaFileSources[IO](path, imports.map(Path.apply)) {
        override def sources: IO[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] = {
          IO.pure(Valid(Chain.one((FileModuleId(path), input))))
        }
      }
    compileRaw(
      aquaConfig,
      strSources,
      backend
    )
  }

  private def compileRaw(
    aquaConfig: AquaAPIConfig,
    sources: AquaSources[IO, AquaFileError, FileModuleId],
    backend: Backend
  ): IO[ValidatedNec[String, Chain[AquaCompiled[FileModuleId]]]] = {

    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).traverseN { (level, constants) =>

      LogFormatter.initLogger(Some(level))

      val config = AquaCompilerConf(constants)
      val transformConfig = aquaConfig.getTransformConfig

      CompilerAPI
        .compile[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          new AirValidator[IO] {
            override def init(): IO[Unit] = Applicative[IO].pure(())
            override def validate(airs: List[AirFunction]): IO[ValidatedNec[String, Unit]] =
              Applicative[IO].pure(validNec(()))
          },
          new Backend.Transform:
            override def transform(ex: AquaContext): AquaRes =
              Transform.contextRes(ex, transformConfig)

            override def generate(aqua: AquaRes): Seq[Generated] = backend.generate(aqua)
          ,
          config
        )
    }.map(_.leftMap(NonEmptyChain.fromNonEmptyList).andThen(identity))
  }
}
