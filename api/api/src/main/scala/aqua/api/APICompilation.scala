package aqua.api

import aqua.Rendering.given
import aqua.raw.value.ValueRaw
import aqua.raw.ConstantRaw
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
import aqua.{AquaIO, SpanParser}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.backend.api.APIBackend
import aqua.definitions.FunctionDef
import aqua.model.AquaContext
import aqua.res.AquaRes

import cats.Applicative
import cats.~>
import cats.data.{Chain, EitherT, NonEmptyChain, Validated, ValidatedNec, Writer, ValidatedNel, NonEmptyList}
import cats.data.Validated.{invalid, invalidNec, validNec, Invalid, Valid}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.syntax.either.*
import fs2.io.file.{Files, Path}
import scribe.{Logging, Level}

object APICompilation {

  extension [A, C <: NonEmptyChain[String] | NonEmptyList[String]](v: Validated[C, A]) {
    def toResult: APIResult[A] = 
      v.toEither.leftMap{
        case v: NonEmptyChain[String] => v
        case v: NonEmptyList[String] => 
          NonEmptyChain.fromNonEmptyList(v)
      }.toEitherT
  }

  extension [A](v: CompileResult[FileModuleId, AquaFileError, FileSpan.F][A]) {
    def toResult: APIResult[A] = 
      v.leftMap(_.map(_.show)).mapK(
        new (CompileWarnings[FileSpan.F] ~> APIWarnings) {
          override def apply[A](w: CompileWarnings[FileSpan.F][A]): APIWarnings[A] =
            w.mapWritten(_.map(_.show))
        }
      )
  }

  def compileCall(
    functionStr: String,
    pathStr: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig,
    fillWithTypes: List[ValueRaw] => ValidatedNec[String, List[ValueRaw]]
  ): IO[APIResult[(FunctionDef, String)]] = {
    given AquaIO[IO] = new AquaFilesIO[IO]

    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).tupled.toResult.flatTraverse { 
      case (level, constants) =>

      val transformConfig = aquaConfig.getTransformConfig.copy(constants = constants)

      LogFormatter.initLogger(Some(level))

      new FuncCompiler[IO](
        Some(RelativePath(Path(pathStr))),
        imports.map(Path.apply),
        transformConfig
      ).compile().map { contextV =>
        for {
          context <- contextV.toResult
          cliFunc <- CliFunc
            .fromString(functionStr)
            .toResult
          arrow <- FuncCompiler
            .findFunction(context, cliFunc)
            .toResult
          argsWithTypes <- fillWithTypes(cliFunc.args).toResult
          func = cliFunc.copy(args = argsWithTypes)
          preparer = new RunPreparer(
                    func,
                    arrow,
                    transformConfig
                  )
          ci <- preparer.prepare().toResult
        } yield ci.definitions -> ci.air
      }
    }
  }

  def compilePath(
    pathStr: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig,
    backend: Backend
  ): IO[APIResult[Chain[AquaCompiled[FileModuleId]]]] = {
    given AquaIO[IO] = new AquaFilesIO[IO]

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
  ): IO[APIResult[Chain[AquaCompiled[FileModuleId]]]] = {
    given AquaIO[IO] = new AquaFilesIO[IO]

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
  ): IO[APIResult[Chain[AquaCompiled[FileModuleId]]]] = 
    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).tupled.toResult.flatTraverse {
      case (level, constants) => 
        LogFormatter.initLogger(Some(level))

      val transformConfig = aquaConfig.getTransformConfig
      val config = AquaCompilerConf(constants ++ transformConfig.constantsList)

      CompilerAPI
        .compile[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          new AirValidator[IO] {
            override def init(): IO[Unit] = Applicative[IO].pure(())
            override def validate(airs: List[AirFunction]): IO[ValidatedNec[String, Unit]] =
              Applicative[IO].pure(validNec(()))
          },
          new Backend.Transform {
            override def transform(ex: AquaContext): AquaRes =
              Transform.contextRes(ex, transformConfig)

            override def generate(aqua: AquaRes): Seq[Generated] = backend.generate(aqua)
          },
          config
        ).map(_.toResult)
    }
}
