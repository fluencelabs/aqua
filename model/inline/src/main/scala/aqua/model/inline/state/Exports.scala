package aqua.model.inline.state

import aqua.model.ValueModel
import aqua.raw.ops.Call
import aqua.raw.value.ValueRaw
import cats.data.State

/**
 * Exports – trace values available in the scope
 * @tparam S
 *   State
 */
trait Exports[S] extends Scoped[S] {
  self =>

  /**
   * [[value]] is accessible as [[exportName]]
   * @param exportName
   *   Name
   * @param value
   *   Value
   */
  def resolved(exportName: String, value: ValueModel): State[S, Unit]

  /**
   * Resolve the whole map of exports
   * @param exports
   *   name -> value
   */
  def resolved(exports: Map[String, ValueModel]): State[S, Unit]

  /**
   * Get all the values available in the scope
   */
  val exports: State[S, Map[String, ValueModel]]

  /**
   * Change [[S]] to [[R]]
   */
  def transformS[R](f: R => S, g: (R, S) => R): Exports[R] = new Exports[R] {

    override def resolved(exportName: String, value: ValueModel): State[R, Unit] =
      self.resolved(exportName, value).transformS(f, g)

    override def resolved(exports: Map[String, ValueModel]): State[R, Unit] =
      self.resolved(exports).transformS(f, g)

    override val exports: State[R, Map[String, ValueModel]] =
      self.exports.transformS(f, g)

    override val purge: State[R, R] =
      self.purgeR(f, g)

    override protected def fill(s: R): State[R, Unit] =
      self.fillR(s, f, g)
  }
}

object Exports {
  def apply[S](implicit exports: Exports[S]): Exports[S] = exports

  object Simple extends Exports[Map[String, ValueModel]] {

    // Exports[Map[NonEmptyList[String], ValueModel]]

    override def resolved(
      exportName: String,
      value: ValueModel
    ): State[Map[String, ValueModel], Unit] = State.modify(_ + (exportName -> value))

    override def resolved(exports: Map[String, ValueModel]): State[Map[String, ValueModel], Unit] =
      State.modify(_ ++ exports)

    override val exports: State[Map[String, ValueModel], Map[String, ValueModel]] =
      State.get

    override val purge: State[Map[String, ValueModel], Map[String, ValueModel]] =
      for {
        s <- State.get
        _ <- State.set(Map.empty)
      } yield s

    override protected def fill(s: Map[String, ValueModel]): State[Map[String, ValueModel], Unit] =
      State.set(s)
  }
}
