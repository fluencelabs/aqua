package aqua.helpers.syntax

import cats.Functor
import cats.data.OptionT
import cats.syntax.functor.*

object optiont {

  extension (o: OptionT.type) {

    /**
     * Lifts a `F[Boolean]` into a `OptionT[F, Unit]` that is `None` if the
     * condition is `false` and `Some(())` otherwise.
     *
     * This is useful for filtering a `OptionT[F, A]` inside a for-comprehension.
     */
    def withFilterF[F[_]: Functor](fb: F[Boolean]): OptionT[F, Unit] =
      OptionT.liftF(fb).filter(identity).void
  }
}
