package aqua.linker

import cats.data.{EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.{Applicative, Monoid}

import java.io.File

case class AquaFiles(
  output: List[String],
  fetched: Map[String, AquaFile],
  unresolvedImports: Set[String],
  source: File,
  importFrom: List[File]
) {

  def addFile[F[_]: Files: Concurrent](
    name: String,
    af: AquaFile
  ): EitherT[F, LinkerError, AquaFiles] = ???

}

object AquaFiles {

  type FetchFiles = ValidatedNec[LinkerError, Map[String, AquaFile]]

  private def emptyFetch[F[_]: Applicative]: F[FetchFiles] =
    Validated.valid[NonEmptyChain[LinkerError], Map[String, AquaFile]](Map.empty).pure[F]

  implicit def fetchFilesMonoid[F[_]: Applicative]: Monoid[F[FetchFiles]] =
    new Monoid[F[FetchFiles]] {
      override def empty: F[FetchFiles] = emptyFetch[F]

      override def combine(x: F[FetchFiles], y: F[FetchFiles]): F[FetchFiles] =
        (x, y).mapN((a, b) => a.andThen(mp => b.map(_ ++ mp)))
    }

  def readSrc[F[_]](
    files: List[File],
    imports: List[File],
    isImportFile: Boolean
  ): F[FetchFiles] =
    files
      .foldLeft[F[FetchFiles]](emptyFetch[F]) {
        case (acc, f) if f.isFile && f.getName.endsWith(".aqua") =>
          for {
            fa <- acc
            v <- AquaFile.read(f).value
          } yield v match {
            case Left(e) =>
              fa.leftMap(_.append(e)).pure[F]
            case Right(af) =>
              fa.map(_.updated(f.getName, af))
          }

        case (acc, f) if f.isDirectory =>
          for {
            fa <- acc
            add <- readSrc(f.listFiles().toList, imports, isImportFile = false)
            unresolvedImports = add.map(mp =>
              mp.values
                .flatMap(_.imports.toList)
                .toSet
                .filterNot(mp.contains)
                .foldLeft[F[FetchFiles]](emptyFetch[F]) { case (importAcc, i) =>
                  imports
                    .map(_.toPath.resolve(i).toFile)
                    .find(_.isFile)
                    .fold[F[FetchFiles]](
                      importAcc.map(
                        _.andThen(_ =>
                          Validated.invalidNec[CliError, Map[String, AquaFile]](
                            UnresolvedImportError(f, i)
                          )
                        )
                      )
                    )(z => readSrc(z :: Nil, imports, isImportFile = true))
                }
            )
          } yield fa.andThen(mp =>
            add
              .map(_.map { case (k, v) =>
                (f.getName + "/" + k) -> v
              })
              .map(mp ++ _)
          )
      }

}
