package aqua.model.func

import cats.data.State

trait Scoped[S] {

  val purge: State[S, S]

  protected def fill(s: S): State[S, Unit]

  def scope[T](scoped: State[S, T]): State[S, T] =
    for {
      r <- purge
      t <- scoped
      _ <- fill(r)
    } yield t

  protected def purgeR[R](f: R => S, g: (R, S) => R): State[R, R] =
    for {
      r <- State.get[R]
      s <- purge.transformS(f, g)
    } yield g(r, s)

  protected def fillR[R](s: R, f: R => S, g: (R, S) => R): State[R, Unit] =
    fill(f(s)).transformS(f, g)
}
