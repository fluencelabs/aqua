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

package aqua.helpers.syntax

import cats.data.OptionT
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Functor, Monad}

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

  extension [F[_], A](o: OptionT[F, A]) {

    /**
     * Like `flatTransform` but the transformation function returns a `OptionT[F, B]`.
     */
    def flatTransformT[B](
      f: Option[A] => OptionT[F, B]
    )(using F: Monad[F]): OptionT[F, B] =
      o.flatTransform(f.andThen(_.value))
  }

  extension [F[_]: Monad, A, B](
    t: Tuple2[OptionT[F, A], OptionT[F, B]]
  ) {

    /**
     * Merges `OptionT`s into `OptionT` of a tuple,
     * **executing both effects**.
     */
    def merged: OptionT[F, (A, B)] = OptionT(
      for {
        a <- t._1.value
        b <- t._2.value
      } yield (a, b).tupled
    )
  }
}
