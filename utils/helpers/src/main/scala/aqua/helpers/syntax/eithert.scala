package aqua.syntax

import cats.Functor
import cats.data.{EitherT, Validated}
import cats.syntax.functor.*

object eithert {

  extension (e: EitherT.type) {

    /**
     * Converts a `F[Validated[A, B]]` into an `EitherT[F, A, B]`.
     */
    def fromValidatedF[F[_]: Functor, A, B](v: F[Validated[A, B]]): EitherT[F, A, B] =
      EitherT(v.map(_.toEither))
  }
}
