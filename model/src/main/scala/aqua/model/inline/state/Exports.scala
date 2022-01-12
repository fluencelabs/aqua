package aqua.model.inline.state

import aqua.raw.ops.Call
import aqua.raw.value.ValueRaw
import cats.data.State

/**
 * Exports – trace values available in the scope
 * @tparam S State
 */
trait Exports[S] extends Scoped[S] {
  self =>

  /**
   * [[value]] is accessible as [[exportName]]
   * @param exportName Name
   * @param value Value
   */
  def resolved(exportName: String, value: ValueRaw): State[S, Unit]

  /**
   * Resolve the whole map of exports
   * @param exports name -> value
   */
  def resolved(exports: Map[String, ValueRaw]): State[S, Unit]

  /**
   * Get all the values available in the scope
   */
  val exports: State[S, Map[String, ValueRaw]]

  /**
   * Put the available resolved values into the [[call]] object
   * @param call Call
   * @return Resolved Call
   */
  final def resolveCall(call: Call): State[S, Call] =
    exports.map(res => call.mapValues(_.resolveWith(res)))

  /**
   * Change [[S]] to [[R]]
   */
  def transformS[R](f: R => S, g: (R, S) => R): Exports[R] = new Exports[R] {

    override def resolved(exportName: String, value: ValueRaw): State[R, Unit] =
      self.resolved(exportName, value).transformS(f, g)

    override def resolved(exports: Map[String, ValueRaw]): State[R, Unit] =
      self.resolved(exports).transformS(f, g)

    override val exports: State[R, Map[String, ValueRaw]] =
      self.exports.transformS(f, g)

    override val purge: State[R, R] =
      self.purgeR(f, g)

    override protected def fill(s: R): State[R, Unit] =
      self.fillR(s, f, g)
  }
}

object Exports {
  def apply[S](implicit exports: Exports[S]): Exports[S] = exports

  object Simple extends Exports[Map[String, ValueRaw]] {

    override def resolved(exportName: String, value: ValueRaw): State[Map[String, ValueRaw], Unit] =
      State.modify(_ + (exportName -> value))

    override def resolved(exports: Map[String, ValueRaw]): State[Map[String, ValueRaw], Unit] =
      State.modify(_ ++ exports)

    override val exports: State[Map[String, ValueRaw], Map[String, ValueRaw]] =
      State.get

    override val purge: State[Map[String, ValueRaw], Map[String, ValueRaw]] =
      for {
        s <- State.get
        _ <- State.set(Map.empty)
      } yield s

    override protected def fill(s: Map[String, ValueRaw]): State[Map[String, ValueRaw], Unit] =
      State.set(s)
  }
}
