package aqua.json

import aqua.types.{ArrayType, BottomType, LiteralType, OptionType, StructType, Type}
import cats.data.{NonEmptyMap, Validated, ValidatedNec}
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.semigroup.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

import scala.collection.immutable.SortedMap
import scala.scalajs.js

object JsonEncoder {

  /* Get widest possible type from JSON arrays. For example:
  JSON: {
          field1: [
                  {
                    a: "a",
                    b: [1,2,3],
                    c: 4
                  },
                  {
                    c: 3
                  }
                  ]
        }
  There type in array must be { a: ?string, b: []number, c: number
   */
  def compareAndGetWidestType(
    name: String,
    ltV: ValidatedNec[String, Type],
    rtV: ValidatedNec[String, Type]
  ): ValidatedNec[String, Type] = {
    (ltV, rtV) match {
      case (Validated.Valid(lt), Validated.Valid(rt)) =>
        (lt, rt) match {
          case (lt, rt) if lt == rt => validNec(lt)
          case (_ @BottomType, ra @ ArrayType(_)) => validNec(ra)
          case (la @ ArrayType(_), _ @BottomType) => validNec(la)
          case (lo @ OptionType(lel), rtt) if lel == rtt => validNec(lo)
          case (ltt, ro @ OptionType(rel)) if ltt == rel => validNec(ro)
          case (_ @BottomType, rb) => validNec(OptionType(rb))
          case (lb, _ @BottomType) => validNec(OptionType(lb))
          case (lst: StructType, rst: StructType) =>
            val lFieldsSM: SortedMap[String, Type] = lst.fields.toSortedMap
            val rFieldsSM: SortedMap[String, Type] = rst.fields.toSortedMap
            val a = lFieldsSM.toList ++ rFieldsSM.toList
            val z: ValidatedNec[String, NonEmptyMap[String, Type]] = a
              .groupBy(_._1)
              .view
              .mapValues(_.map(_._2))
              .map {
                case (name, t :: Nil) =>
                  compareAndGetWidestType(name, validNec(t), validNec(BottomType)).map(t =>
                    (name, t)
                  )
                case (name, lt :: rt :: Nil) =>
                  compareAndGetWidestType(name, validNec(lt), validNec(rt)).map(t => (name, t))
                case _ => invalidNec("Unexpected")
              }
              .toList
              .sequence
              .map(processedFields => NonEmptyMap.fromMap(SortedMap(processedFields: _*)).get)
            z.map(mt => StructType("", mt))
          case (a, b) =>
            invalidNec(s"Types in '$name' array should be the same")
        }
      case (Validated.Invalid(lerr), Validated.Invalid(rerr)) =>
        Validated.Invalid(lerr ++ rerr)
      case (l @ Validated.Invalid(_), _) =>
        l
      case (_, r @ Validated.Invalid(_)) =>
        r
    }
  }

  // Gather all information about all fields in JSON and create Aqua type.
  def aquaTypeFromJson(name: String, arg: js.Dynamic): ValidatedNec[String, Type] = {
    val t = js.typeOf(arg)
    arg match {
      case a if t == "string" => validNec(LiteralType.string)
      case a if t == "number" => validNec(LiteralType.number)
      case a if t == "boolean" => validNec(LiteralType.bool)
      case a if js.Array.isArray(a) =>
        // if all types are similar it will be array array with this type
        // otherwise array with bottom type
        val elementsTypesV: ValidatedNec[String, List[Type]] =
          a.asInstanceOf[js.Array[js.Dynamic]].map(ar => aquaTypeFromJson(name, ar)).toList.sequence

        elementsTypesV.andThen { elementsTypes =>
          println("raw: " + elementsTypes)
          if (elementsTypes.isEmpty) validNec(ArrayType(BottomType))
          else {
            elementsTypes
              .map(el => validNec(el))
              .reduce[ValidatedNec[String, Type]] { case (l, t) =>
                compareAndGetWidestType(name, l, t)
              }
              .map(t => ArrayType(t))
          }
        }
      case a if t == "object" && !js.isUndefined(arg) && arg != null =>
        val dict = arg.asInstanceOf[js.Dictionary[js.Dynamic]]
        val keys = dict.keys
        keys
          .map(k => aquaTypeFromJson(k, arg.selectDynamic(k)).map(t => k -> t))
          .toList
          .sequence
          .map { fields =>
            StructType("", NonEmptyMap.fromMap(SortedMap(fields: _*)).get)
          }

      case _ => validNec(BottomType)
    }
  }
}
