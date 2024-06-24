/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.api

import aqua.Rendering.given
import aqua.api.AquaAPIConfig
import aqua.backend.api.APIBackend
import aqua.backend.{AirFunction, Backend, Generated}
import aqua.compiler.*
import aqua.constants.Constants
import aqua.definitions.FunctionDef
import aqua.files.{AquaFileSources, AquaFilesIO, AquaStringSources, FileModuleId}
import aqua.io.*
import aqua.logging.{LogFormatter, LogLevels}
import aqua.model.AquaContext
import aqua.model.transform.{Transform, TransformConfig}
import aqua.parser.expr.AbilityExpr.p
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, LexerError, ParserError}
import aqua.raw.ConstantRaw
import aqua.raw.ops.Call
import aqua.raw.value.ValueRaw
import aqua.res.AquaRes
import aqua.run.{CliFunc, FuncCompiler, RunPreparer}
import aqua.{AquaIO, SpanParser}

import cats.Applicative
import cats.data.*
import cats.data.Validated.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.~>
import fs2.io.file.{Files, Path}
import scribe.{Level, Logging}

object APICompilation {

  def compileCall(
    functionStr: String,
    pathStr: String,
    imports: Imports,
    aquaConfig: AquaAPIConfig,
    fillWithTypes: List[ValueRaw] => ValidatedNec[String, List[ValueRaw]]
  ): IO[APIResult[(FunctionDef, String)]] = {
    given AquaIO[IO] = new AquaFilesIO[IO]

    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).tupled.toResult.flatTraverse { case (level, constants) =>
      val transformConfig = aquaConfig.getTransformConfig
      val config = aquaConfig.getCompilerConfig.copy(constants = constants)

      LogFormatter.initLogger(Some(level))

      new FuncCompiler[IO](
        Some(RelativePath(Path(pathStr))),
        imports.toIO,
        transformConfig,
        config
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
    imports: Imports,
    aquaConfig: AquaAPIConfig,
    backend: Backend
  ): IO[APIResult[Chain[AquaCompiled[FileModuleId]]]] = {
    given AquaIO[IO] = new AquaFilesIO[IO]

    val path = Path(pathStr)
    val sources = new AquaFileSources[IO](
      path,
      imports.toIO
    )

    compileRaw(
      aquaConfig,
      sources,
      backend
    )
  }

  def compileString(
    input: String,
    imports: Imports,
    aquaConfig: AquaAPIConfig,
    backend: Backend
  ): IO[APIResult[Chain[AquaCompiled[FileModuleId]]]] = {
    given AquaIO[IO] = new AquaFilesIO[IO]

    val path = Path("")

    val strSources: AquaStringSources[IO] =
      new AquaStringSources(
        Map(FileModuleId(path) -> input),
        imports.toIO
      )

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
  ): IO[APIResult[Chain[AquaCompiled[FileModuleId]]]] = (
    LogLevels.levelFromString(aquaConfig.logLevel),
    Constants.parse(aquaConfig.constants)
  ).tupled.toResult.flatTraverse { case (level, constants) =>
    LogFormatter.initLogger(Some(level))

    val transformConfig = aquaConfig.getTransformConfig
    val config = aquaConfig.getCompilerConfig.copy(constants = constants)

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
      )
      .map(_.toResult)
  }

  extension [A](v: ValidatedNec[String, A]) {

    def toResult: APIResult[A] =
      v.toEither.toEitherT
  }

  extension [A](v: CompileResult[FileModuleId, AquaFileError, FileSpan.F][A]) {

    def toResult: APIResult[A] =
      v.leftMap(_.map(_.show))
        .mapK(
          new (CompileWarnings[FileSpan.F] ~> APIWarnings) {

            override def apply[A](w: CompileWarnings[FileSpan.F][A]): APIWarnings[A] =
              w.mapWritten(_.map(_.show))
          }
        )
  }
}
