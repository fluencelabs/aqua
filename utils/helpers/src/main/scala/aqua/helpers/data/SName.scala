package aqua.helpers.data

import aqua.errors.Errors.internalError

/**
 * Short for SimpleName. Represents name without `.`
 */
final case class SName private (
  name: String
) {

  lazy val toPName: PName =
    PName.fromSName(this)
}

object SName {

  def nameUnsafe(name: String): SName =
    if (name.isEmpty || name.contains("."))
      internalError(s"Invalid SName: $name")
    else SName(name)
}
