package aqua.json

import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import cats.data.Validated.{invalidNec, validNec}
import cats.data.{Validated, ValidatedNec}
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.partialOrder.*
import cats.syntax.show.*
import cats.syntax.traverse.*

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSON

object TypeValidator {

  /**
   * Compare and validate type from Aqua file and type generated from JSON.
   *    Also, the validation will succeed if the JSON type is missing an array or an optional field.
   *    The result will be incorrect if left and right are the same type.
   * @param name field name
   * @param aquaType a type from Aqua code
   * @param jsonType a type generated from JSON
   * @param fullOptionType save full optional part of Aqua and JSON field types to print clear error message if needed
   * @return
   */
  def validateTypes(
    name: String,
    aquaType: Type,
    jsonType: Option[Type],
    fullOptionType: Option[(Type, Type)] = None
  ): ValidatedNec[String, Unit] = {
    jsonType match {
      case None =>
        aquaType match {
          case _: OptionType =>
            validNec(())
          case _ =>
            invalidNec(s"Missing field '$name' in arguments")
        }
      case Some(rt) =>
        (aquaType, rt) match {
          case (l: ProductType, r: ProductType) =>
            val ll = l.toList
            val rl = r.toList

            if (ll.length != rl.length)
              invalidNec(
                s"Type of the field '$name' is incorrect. Expected: '$l' Actual: '$r'"
              )
            else
              ll.zip(rl)
                .map { case (aquaType, rt) =>
                  validateTypes(s"$name", aquaType, Some(rt))
                }
                .sequence
                .map(_ => ())
          case (l: StructType, r: StructType) =>
            val lsm: SortedMap[String, Type] = l.fields.toSortedMap
            val rsm: SortedMap[String, Type] = r.fields.toSortedMap

            lsm.map { case (n, ltt) =>
              validateTypes(s"$name.$n", ltt, rsm.get(n))
            }.toList.sequence.map(_ => ())
          case (l: OptionType, r) =>
            // if we have ?[][]string and [][][]string it must throw an error
            validateTypes(name, l.element, Some(r), Some((l, r)))
          case (l: BoxType, r: BoxType) =>
            validateTypes(name, l.element, Some(r.element), fullOptionType.orElse(Some(l, r)))

          case (l, r) =>
            if (l >= r) validNec(())
            else
              val (li, ri) = fullOptionType.getOrElse((l, r))
              invalidNec(
                s"Type of the field '$name' is incorrect. Expected: '$li' Actual: '$ri'"
              )
        }
    }
  }
}
