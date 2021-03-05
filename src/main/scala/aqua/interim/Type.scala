package aqua.interim

import cats.PartialOrder
import cats.data.NonEmptyMap

sealed trait Type
sealed trait DataType extends Type

case class ScalarType private (name: String) extends DataType

object ScalarType {
  val i32 = ScalarType("i32")
  val i64 = ScalarType("i64")
  val s32 = ScalarType("s32")
  val s64 = ScalarType("s64")
  val f32 = ScalarType("f32")
  val f64 = ScalarType("f64")
  val bool = ScalarType("bool")
  val string = ScalarType("string")

  val float = Set(f32, f64)
  val signed = float ++ Set(s32, s64)
  val number = signed ++ Set(i32, i64)
  val all = number ++ Set(bool, string)

  val boolSet = Set(bool)
  val stringSet = Set(string)

  val scalarOrder: PartialOrder[ScalarType] =
    PartialOrder.from {
      case (a, b) if a == b => 0.0
      case (`i32`, `i64`) => -1.0
      case (`s32`, `s64`) => -1.0
      case (`f32`, `f64`) => -1.0
      case (`i64`, `i32`) => 1.0
      case (`s64`, `s32`) => 1.0
      case (`f64`, `f32`) => 1.0
      case _ => Double.NaN
    }
}

case class ArrayType(element: DataType) extends DataType
case class ProductType(name: String, fields: NonEmptyMap[String, DataType]) extends DataType

case class ArrowType(args: List[DataType], res: Option[DataType]) extends Type
case class FuncArrowType(args: List[(String, Either[ArrowType, DataType])], res: Option[DataType]) extends Type

object Type {}
