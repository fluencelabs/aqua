package aqua.helpers.data

import aqua.errors.Errors.internalError

import cats.data.NonEmptyList
import cats.syntax.option.*

/**
 * Short for PathName. Represents name with parts separated by `.`
 */
final case class PName private (
  parts: NonEmptyList[SName]
) {

  def head: SName = parts.head

  def tail: Option[PName] = NonEmptyList.fromList(parts.tail).map(PName.apply)

  def uncons: (SName, Option[PName]) =
    parts.head -> NonEmptyList.fromList(parts.tail).map(PName.apply)

  def unconsR: (Option[PName], SName) =
    NonEmptyList.fromList(parts.init).map(PName.apply) -> parts.last

  def prefixed(prefix: SName): PName =
    PName(parts.prepend(prefix))

  def postfixed(postfix: SName): PName =
    PName(parts.append(postfix))

  def prepended(prepend: PName): PName =
    PName(prepend.parts.concatNel(parts))

  def startsWith(path: PName): Boolean =
    if (path.parts.length > parts.length) false
    else path.parts.zip(parts).forall { case (a, b) => a == b }

  def replacePrefix(prefix: PName, replace: PName): PName =
    if (!startsWith(prefix)) internalError(s"Cannot replace $prefix in $this")
    else PName(replace.parts.appendList(parts.toList.drop(prefix.parts.length)))

  lazy val simple: Option[SName] =
    Option.when(parts.length == 1)(parts.head)

  lazy val isSimple: Boolean = simple.isDefined

  lazy val value: String = parts.toList.map(_.name).mkString(".")

  lazy val splits: List[(PName, PName)] = {
    val partsList = parts.toList
    (1 until parts.length).toList.map(i =>
      PName(NonEmptyList.fromListUnsafe(partsList.take(i))) ->
        PName(NonEmptyList.fromListUnsafe(partsList.drop(i)))
    )
  }

  override def toString(): String = value
}

object PName {

  def fromSName(name: SName): PName =
    PName(NonEmptyList.one(name))

  def stringUnsafe(name: String): PName =
    NonEmptyList
      .fromList(name.split("\\.").toList)
      .map(partsUnsafe)
      .getOrElse(
        internalError(s"Incorrect PName: $name")
      )

  def partsUnsafe(parts: NonEmptyList[String]): PName =
    PName(parts.map(SName.nameUnsafe))

  def simpleUnsafe(name: String): PName =
    PName(NonEmptyList.one(SName.nameUnsafe(name)))
}
