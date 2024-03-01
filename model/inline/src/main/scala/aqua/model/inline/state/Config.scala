package aqua.model.inline.state

import cats.data.{Reader, State}

/**
 * Representation that `S` contains configuration for inlining
 */
trait Config[S] {
  self =>

  /**
   * Flag that disables error propagation mechanics in inlined code
   */
  def noErrorPropagation: Reader[S, Boolean]

  final def transform[R](f: R => S): Config[R] = new Config[R] {

    override def noErrorPropagation: Reader[R, Boolean] =
      self.noErrorPropagation.local(f)
  }
}

object Config {
  case class Values(noErrorPropagation: Boolean = false)

  object Values {
    lazy val default: Values = Values()
  }

  given Config[Values] = new Config[Values] {

    override def noErrorPropagation: Reader[Values, Boolean] =
      Reader(_.noErrorPropagation)
  }

  def apply[S: Config]: Config[S] =
    implicitly[Config[S]]

  def noErrorPropagation[S: Config]: Reader[S, Boolean] =
    Config[S].noErrorPropagation
}
