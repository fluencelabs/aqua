package aqua

import aqua.backend.{Backend, Generated}
import aqua.compiler.{AirValidator, AquaCompiled, AquaCompiler, AquaCompilerConf, AquaError, CompilerAPI}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io.*
import aqua.air.AirValidation
import aqua.backend.AirString
import aqua.model.AquaContext
import aqua.model.transform.TransformConfig
import aqua.model.transform.Transform
import aqua.parser.lift.LiftParser.LiftErrorOps
import aqua.parser.lift.Span.spanLiftParser
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import aqua.parser.{Ast, LexerError, Parser}
import aqua.raw.ConstantRaw
import aqua.res.AquaRes
import cats.data.*
import cats.effect.Async
import cats.parse.LocationMap
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.show.*
import cats.{Applicative, Eval, Monad, Show, ~>}
import fs2.io.file.{Files, Path}
import scribe.Logging

object AquaPathCompiler extends Logging {

  /**
   * @param srcPath path to aqua sources
   * @param imports additional paths to possible aqua imports
   * @param targetPath path where compiled files will be created. Creates no output if empty
   * @param backend creates output (TS, JS, ...) from a model
   * @param transformConfig transformation configuration for a model
   * @return errors or result messages
   */
  def compileFilesTo[F[_]: AquaIO: Monad: Files: Async](
    srcPath: Path,
    imports: List[Path],
    targetPath: Option[Path],
    backend: Backend,
    transformConfig: TransformConfig
  ): F[ValidatedNec[String, Chain[String]]] = {
    import ErrorRendering.showError
    (for {
      prelude <- Prelude.init()
      sources = new AquaFileSources[F](srcPath, imports ++ prelude.importPaths)
      compiler <- CompilerAPI
        .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
          sources,
          SpanParser.parser,
          new AirValidator[F] {
            override def init(): F[Unit] = AirValidation.init[F]()

            override def validate(
              airs: List[AirString]
            ): F[ValidatedNec[String, Unit]] = AirValidation.validate[F](airs)
          },
          new Backend.Transform:
            override def transform(ex: AquaContext): AquaRes =
              Transform.contextRes(ex, transformConfig)

            override def generate(aqua: AquaRes): Seq[Generated] = backend.generate(aqua)
          ,
          AquaCompilerConf(transformConfig.constantsList),
          targetPath.map(sources.write).getOrElse(dry[F])
        )
    } yield {
      compiler
      // 'distinct' to delete all duplicated errors
    }).map(_.leftMap(_.map(_.show).distinct))

  }

  def dry[F[_]: Applicative](
    ac: AquaCompiled[FileModuleId]
  ): F[Seq[Validated[AquaFileError, String]]] =
    Seq(
      Validated.valid[AquaFileError, String](
        s"Source ${ac.sourceId.file}: compilation OK"
      )
    ).pure[F]

}
