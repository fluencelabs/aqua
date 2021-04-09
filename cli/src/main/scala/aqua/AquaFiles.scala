package aqua

import aqua.cli.CliError
import aqua.parser.Ast
import aqua.parser.lift.FileSpan
import cats.Applicative
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.effect.kernel.Concurrent
import fs2.io.file.Files
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._

import java.io.File
import java.nio.file.Path

case class AquaFiles(
  output: List[String],
  fetched: Map[String, AquaFile],
  unresolvedImports: Set[String]
) {

  def addFile(name: String, af: AquaFile): AquaFiles = ???

}

object AquaFiles {

  type FetchFiles = ValidatedNec[CliError, Map[String, AquaFile]]

  private def emptyFetch[F[_]: Applicative]: F[FetchFiles] =
    Validated.valid[NonEmptyChain[CliError], Map[String, AquaFile]](Map.empty).pure[F]

  def readSrc[F[_]: Concurrent: Files](
    files: List[File],
    imports: List[File]
  ): F[FetchFiles] =
    files
      .foldLeft[F[FetchFiles]](emptyFetch[F]) {
        case (acc, f) if f.isFile && f.getName.endsWith(".aqua") =>
          for {
            fa <- acc
            v <- AquaFile.read(f).value
          } yield v match {
            case Left(e) => fa.leftMap(_.append(e)).pure[F]
            case Right(af) =>
              fa.map(_.updated(f.getName, af))
          }

        case (acc, f) if f.isDirectory =>
          for {
            fa <- acc
            add <- readSrc(f.listFiles().toList)
          } yield fa.andThen(mp =>
            add
              .map(_.map { case (k, v) =>
                (f.getName + "/" + k) -> v
              })
              .map(mp ++ _)
          )
      }

}
