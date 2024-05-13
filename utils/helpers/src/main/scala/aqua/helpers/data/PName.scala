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

  def uncons: (SName, Option[PName]) =
    parts.head -> NonEmptyList.fromList(parts.tail).map(PName.apply)

  def unconsR: (Option[PName], SName) =
    NonEmptyList.fromList(parts.init).map(PName.apply) -> parts.last

  def prefixed(prefix: SName): PName =
    PName(parts.prepend(prefix))

  def prepended(prepend: PName): PName =
    PName(prepend.parts.concatNel(parts))

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

  def partsUnsafe(parts: NonEmptyList[String]): PName =
    PName(parts.map(SName.nameUnsafe))

  def simpleUnsafe(name: String): PName =
    PName(NonEmptyList.one(SName.nameUnsafe(name)))
}
