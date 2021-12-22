package aqua

import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.effect.Concurrent
import fs2.io.file.{Files, Path}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*

import scala.scalajs.js
import scala.scalajs.js.JSON

object OptUtils {

  // Checks if a path is a file and it exists and transforms it
  def checkAndTransformFile[F[_]: Files: Concurrent, T](
    path: String,
    transform: Path => F[ValidatedNec[String, T]]
  ): F[ValidatedNec[String, T]] = {
    val p = Path(path)
    Files[F]
      .exists(p)
      .flatMap { exists =>
        if (exists)
          Files[F].isRegularFile(p).flatMap { isFile =>
            if (isFile) {
              transform(p)
            } else {
              invalidNec(s"Path '${p.toString}' is not a file").pure[F]
            }
          }
        else {
          invalidNec(s"There is no path '${p.toString}'").pure[F]
        }
      }
  }
}
