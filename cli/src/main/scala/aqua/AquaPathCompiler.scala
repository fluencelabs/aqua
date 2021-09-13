package aqua

import aqua.backend.Backend
import aqua.compiler.{AquaCompiled, AquaCompiler, AquaError}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io.*
import aqua.model.transform.TransformConfig
import aqua.parser.{Ast, LexerError}
import aqua.parser.lift.FileSpan
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.show.*
import cats.{Eval, Monad, Show, ~>, Applicative}
import scribe.Logging
import fs2.io.file.{Files, Path}
import aqua.parser.lift.{LiftParser, Span}
import cats.parse.LocationMap
import cats.~>
import aqua.parser.lift.LiftParser.LiftErrorOps
import Span.spanLiftParser
import aqua.parser.Parser

object AquaPathCompiler extends Logging {

  def compileFilesTo[F[_]: AquaIO: Monad: Files](
    srcPath: Path,
    imports: List[Path],
    targetPath: Path,
    backend: Backend,
    bodyConfig: TransformConfig,
    isDryRun: Boolean
  ): F[ValidatedNec[String, Chain[String]]] = {
    import ErrorRendering.showError
    val sources = new AquaFileSources[F](srcPath, imports)
    AquaCompiler
      .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
        sources,
        id => {
          source => {
            val nat = new (Span.F ~> FileSpan.F) {
              override def apply[A](span: Span.F[A]): FileSpan.F[A] = {
                (
                  FileSpan(id.file.absolute.toString, Eval.later(LocationMap(source)), span._1),
                  span._2
                )
              }
            }
            import Span.spanLiftParser
            Parser.natParser(Parser.spanParser, nat)(source)
          }
        },
        backend,
        bodyConfig,
        {if (isDryRun) dry[F] else sources.write(targetPath)}
      )
      .map(_.leftMap(_.map(_.show)))
  }

  def dry[F[_]: Applicative](ac: AquaCompiled[FileModuleId]): F[Seq[Validated[AquaFileError, String]]] =
    Seq(
      Validated.valid[AquaFileError, String](
        s"Source ${ac.sourceId.file}: compilation OK"
      )
    ).pure[F]

}
