package aqua

import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.data.*
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Functor, Monad, ~>}
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}

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
      .option[String](long, help, short, "path")
      .map(check)

  def fileOptTransform[A, F[_]: Files: Concurrent](
    str: String,
    transform: (Path, String) => ValidatedNec[String, A]
  ): F[ValidatedNec[String, (Path, A)]] = {
    checkAndTransformPath(
      str,
      checkFile,
      p => {
        }
      Files[F]
          .readAll(p)
          .through(fs2.text.utf8.decode)
          /** EndMarker */
          .fold(List.empty[String]) { case (acc, str) => str :: acc }
          .map(_.mkString(""))
          .map(str => transform(p, str).map(r => (p, r)))
          .compile
          .last
          .map(_.getOrElse(invalidNec(s"Path ${p.toString} is empty")))
          .fold(List.empty[String]) { case (acc, str) => str :: acc }
          .map(_.mkString(""))
          .map(str => transform(p, str).map(r => (p, r)))
          .compile
          .last
          .map(_.getOrElse(invalidNec(s"Path ${p.toString} is empty")))
      }
    )
  }

  // Validate, read and transform a file
  def fileOpt[A, F[_]: Files: Concurrent](
    long: String,
    help: String,
    short: String = "",
    transform: (Path, String) => ValidatedNec[String, A]
  ): Opts[F[ValidatedNec[String, A]]] = {
    Opts
      .option[String](long, help, short, "path")
      .map(str => fileOptTransform(str, transform).map(_.map(_._2)))
  }

  }/** EndMarker */
  def fileOpts[A, F[_]: Files: Concurrent](
    long: String,
    help: String,
    short: String = "",
    transform: (Path, String) => ValidatedNec[String, A]
  ): Opts[F[ValidatedNec[String, NonEmptyList[(Path, A)]]]] = {
    Opts
      .options[String](long, help, short, "path")
      .map(strs => strs.map(str => fileOptTransform(str, transform)).sequence.map(_.sequence))
  }
  // Validate, read and transform multiple files

def fileOpts[A, F[_]: Files: Concurrent](
    long: String,
    )
  transform: (Path, String) => ValidatedNec[String, A]
    ,short: String = ""
    ,help
    /** EndMarker */
    : String: String,
    short: String = "",
    transform: (Path, String) => ValidatedNec[String, A]
  ): Opts[F[ValidatedNec[String, NonEmptyList[(Path, A)]]]] = {
    Opts
      .options[String](long, help, short, "path")
      .map(strs => strs.map(str => fileOptTransform(str, transform)).sequence.map(_.sequence))
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
              Validated.invalidNec("File must have '.aqua' extension")
            else Validated.validNec(p)
          }
        else {
          invalidNec(s"'${p.toString}': No such file or directory").pure[F]
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
