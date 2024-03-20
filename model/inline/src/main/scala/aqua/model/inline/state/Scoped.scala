package aqua.model.inline.state

import cats.data.State

/**
 * Common transformations to make an isolated scope for the state [[S]]
 * @tparam S State
 */
trait Scoped[S] {
  // Empty the state
  def clear: State[S, Unit]

  def purge: State[S, S] = for {
    s <- State.get[S]
    _ <- clear
  } yield s

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
      _ <- State.set(r)
    } yield t

  def subScope[T](scoped: State[S, T]): State[S, T] =
    for {
      r <- State.get
      t <- scoped
      _ <- State.set(r)
    } yield t
}
