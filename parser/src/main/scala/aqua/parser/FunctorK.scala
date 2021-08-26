package aqua.parser

import shapeless3.deriving.K11

type ~>[A[_], B[_]] = [t] => A[t] => B[t]

trait FunctorK[H[_[_]]] {
  def mapK[A[_], B[_]](af: H[A])(f: A ~> B): H[B]
}

object FunctorK {
  inline def apply[H[_[_]]](using fh: FunctorK[H]): FunctorK[H] = fh

  given [T]: FunctorK[K11.Id[T]] with
    def mapK[A[_], B[_]](at: A[T])(f: A ~> B): B[T] = f(at)

  given functorKGen[H[_[_]]](using inst: => K11.Instances[FunctorK, H]): FunctorK[H] with
    def mapK[A[_], B[_]](ha: H[A])(f: A ~> B): H[B] =
      inst.map(ha)([t[_[_]]] => (ft: FunctorK[t], ta: t[A]) => ft.mapK(ta)(f))

  given [T]: FunctorK[K11.Const[T]] with
    def mapK[A[_], B[_]](t: T)(f: A ~> B): T = t

  inline def derived[F[_[_]]](using gen: K11.Generic[F]): FunctorK[F] = functorKGen
}
