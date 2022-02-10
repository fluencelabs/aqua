package aqua

import cats.data.{Validated, ValidatedNec}
import cats.effect.Concurrent
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.{~>, Comonad, Functor, Monad}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}

object FileOpts {

  // Validate, read and transform a file
  def fileOptionalOpt[A, F[_]: Files: Concurrent](
    long: String,
    help: String,
    short: String = "",
    transform: (Path, String) => ValidatedNec[String, A]
  ): Opts[Option[F[ValidatedNec[String, A]]]] = {
    AppOpts.wrapWithOption(fileOpt(long, help, short, transform))
  }

  // Get and validate path
  def pathOpt[A, F[_]: Files: Monad](
    long: String,
    help: String,
    short: String = "",
    check: String => F[ValidatedNec[String, Path]]
  ): Opts[F[ValidatedNec[String, Path]]] =
    Opts
      .option[String](long, help, short)
      .map(check)

  // Validate, read and transform a file
  def fileOpt[A, F[_]: Files: Concurrent](
    long: String,
    help: String,
    short: String = "",
    transform: (Path, String) => ValidatedNec[String, A]
  ): Opts[F[ValidatedNec[String, A]]] = {
    Opts
      .option[String](long, help, short)
      .map { str =>
        checkAndTransformPath(
          str,
          checkFile,
          p => {
            Files[F]
              .readAll(p)
              .through(fs2.text.utf8.decode)
              .fold(List.empty[String]) { case (acc, str) => str :: acc }
              .map(_.mkString(""))
              .map(str => transform(p, str))
              .compile
              .last
              .map(_.getOrElse(invalidNec(s"Path ${p.toString} is empty")))
          }
        )
      }
  }

  // Checks if the path is a file and it exists
  def checkFile[F[_]: Files: Monad](path: String): F[ValidatedNec[String, Path]] = {
    val p = Path(path)
    Files[F]
      .exists(p)
      .flatMap { exists =>
        if (exists)
          Files[F].isRegularFile(p).map { isFile =>
            if (isFile) {
              validNec(p)
            } else {
              invalidNec(s"Path '${p.toString}' is not a file")
            }
          }
        else {
          invalidNec(s"There is no path '${p.toString}'").pure[F]
        }
      }
  }

  // Checks if the path is a file or a directory and it exists
  def checkDirOrFile[F[_]: Files: Monad](
    path: String,
    isAqua: Boolean = true
  ): F[ValidatedNec[String, Path]] = {
    val p = Path(path)
    Files[F]
      .exists(p)
      .flatMap { exists =>
        if (exists)
          Files[F].isRegularFile(p).map { isFile =>
            if (isFile && p.extName != ".aqua")
              Validated.invalidNec("File must be with 'aqua' extension")
            else Validated.validNec(p)
          }
        else {
          invalidNec(s"There is no path '${p.toString}'").pure[F]
        }
      }
  }

  // Checks a path and transforms it
  def checkAndTransformPath[F[_]: Files: Concurrent, T](
    path: String,
    check: String => F[ValidatedNec[String, Path]],
    transform: Path => F[ValidatedNec[String, T]]
  ): F[ValidatedNec[String, T]] = {
    check(path).flatMap {
      case Validated.Valid(p) => transform(p)
      case i @ Validated.Invalid(_) => i.pure[F]
    }
  }
}
