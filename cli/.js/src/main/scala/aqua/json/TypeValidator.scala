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

  // Compare and validate type generated from JSON and type from Aqua file.
  // Also, validation will be success if array or optional field will be missed in JSON type
  def validateTypes(name: String, lt: Type, rtOp: Option[Type]): ValidatedNec[String, Unit] = {
    rtOp match {
      case None =>
        lt match {
          case _: BoxType =>
            validNec(())
          case _ =>
            invalidNec(s"Missing field '$name' in arguments")
        }
      case Some(rt) =>
        (lt, rt) match {
          case (l: ProductType, r: ProductType) =>
            val ll = l.toList
            val rl = r.toList

            if (ll.length != rl.length)
              invalidNec(
                s"Type of the field '$name' is incorrect. Expected: '$l' Actual: '$r'"
              )
            else
              ll.zip(rl)
                .map { case (lt, rt) =>
                  validateTypes(s"$name", lt, Some(rt))
                }
                .sequence
                .map(_ => ())
          case (l: StructType, r: StructType) =>
            val lsm: SortedMap[String, Type] = l.fields.toSortedMap
            val rsm: SortedMap[String, Type] = r.fields.toSortedMap

            lsm.map { case (n, ltt) =>
              validateTypes(s"$name.$n", ltt, rsm.get(n))
            }.toList.sequence.map(_ => ())
          case (l: BoxType, r: BoxType) =>
            validateTypes(name, l.element, Some(r.element))
          case (l: BoxType, r) =>
            validateTypes(name, l.element, Some(r))
          case (l, r) =>
            if (l >= r) validNec(())
            else
              invalidNec(
                s"Type of the field '$name' is incorrect. Expected: '$l' Actual: '$r'"
              )
        }
    }
  }
}
