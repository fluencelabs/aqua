package aqua.model.inline.state

import cats.data.State

/**
 * Common transformations to make an isolated scope for the state [[S]]
 * @tparam S State
 */
trait Scoped[S] {
  // Remove everything from the state
  val purge: State[S, S]

  // Put [[s]] to the state
  protected def fill(s: S): State[S, Unit]

  /**
   * Clear the state, run [[scoped]], then recover the initial state
   * @param scoped What to run with empty [[S]]
   * @tparam T Return type
   * @return Value returned by [[scoped]]
   */
  def scope[T](scoped: State[S, T]): State[S, T] =
    for {
      r <- purge
      t <- scoped
      _ <- fill(r)
    } yield t

  // For transformS
  protected def purgeR[R](f: R => S, g: (R, S) => R): State[R, R] =
    for {
      r <- State.get[R]
      s <- purge.transformS(f, g)
    } yield g(r, s)

  // For transformS
  protected def fillR[R](s: R, f: R => S, g: (R, S) => R): State[R, Unit] =
    fill(f(s)).transformS(f, g)
}
