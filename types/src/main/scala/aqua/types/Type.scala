package aqua.types

import cats.PartialOrder
import cats.data.NonEmptyMap
import cats.instances.option._
import cats.syntax.apply._

sealed trait Type {

  def acceptsValueOf(incoming: Type): Boolean = {
    import Type.typesPartialOrder
    import cats.syntax.partialOrder._
    this >= incoming
  }
}
sealed trait DataType extends Type

case class ScalarType private (name: String) extends DataType {
  override def toString: String = s"Scalar($name)"
}

object ScalarType {
  // TODO https://github.com/fluencelabs/interface-types/blob/master/crates/it-types/src/values.rs#L45-L49
  val u8 = ScalarType("u8")
  val u32 = ScalarType("u32")
  val u64 = ScalarType("u64")
  val s32 = ScalarType("s32")
  val s64 = ScalarType("s64")
  val f32 = ScalarType("f32")
  val f64 = ScalarType("f64")
  val bool = ScalarType("bool")
  val string = ScalarType("string")

  val float = Set(f32, f64)
  val signed = float ++ Set(s32, s64)
  val number = signed ++ Set(u8, u32, u64)
  val all = number ++ Set(bool, string)

  val scalarOrder: PartialOrder[ScalarType] =
    PartialOrder.from {
      case (a, b) if a == b => 0.0
      case (`u32`, `u64`) => -1.0
      case (`u8`, `u64`) => -1.0
      case (`u8`, `u32`) => -1.0
      case (`s32`, `s64`) => -1.0
      case (`f32`, `f64`) => -1.0
      case (`u64`, `u32`) => 1.0
      case (`s64`, `s32`) => 1.0
      case (`f64`, `f32`) => 1.0
      case _ => Double.NaN
    }
}

case class LiteralType private (oneOf: Set[ScalarType], name: String) extends DataType {
  override def toString: String = s"Literal($name)"
}

object LiteralType {
  val float = LiteralType(ScalarType.float, "float")
  val signed = LiteralType(ScalarType.signed, "signed")
  val number = LiteralType(ScalarType.number, "number")
  val bool = LiteralType(Set(ScalarType.bool), "bool")
  val string = LiteralType(Set(ScalarType.string), "string")
}

case class ArrayType(element: Type) extends DataType {
  override def toString: String = "[]" + element
}

case class ProductType(name: String, fields: NonEmptyMap[String, Type]) extends DataType {

  override def toString: String =
    s"$name{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

case class ArrowType(args: List[Type], res: Option[Type]) extends Type {

  def acceptsAsArguments(valueTypes: List[Type]): Boolean =
    (args.length == valueTypes.length) && args
      .zip(valueTypes)
      .forall(av => av._1.acceptsValueOf(av._2))

  override def toString: String =
    args.map(_.toString).mkString(", ") + " -> " + res.map(_.toString).getOrElse("()")
}

case class StreamType(element: Type) extends DataType

object Type {
  import Double.NaN

  private def cmpTypesList(l: List[Type], r: List[Type]): Double =
    if (l.length != r.length) NaN
    else if (l == r) 0.0
    else
      (l zip r).map(lr => cmp(lr._1, lr._2)).fold(0.0) {
        case (a, b) if a == b => a
        case (`NaN`, _) => NaN
        case (_, `NaN`) => NaN
        case (0, b) => b
        case (a, 0) => a
        case _ => NaN
      }

  private def cmpProd(lf: NonEmptyMap[String, Type], rf: NonEmptyMap[String, Type]): Double =
    if (lf.toSortedMap == rf.toSortedMap) 0.0
    else if (
      lf.keys.forall(rf.contains) && cmpTypesList(
        lf.toSortedMap.toList.map(_._2),
        rf.toSortedMap.view.filterKeys(lf.keys.contains).toList.map(_._2)
      ) == -1.0
    ) -1.0
    else if (
      rf.keys.forall(lf.contains) && cmpTypesList(
        lf.toSortedMap.view.filterKeys(rf.keys.contains).toList.map(_._2),
        rf.toSortedMap.toList.map(_._2)
      ) == 1.0
    ) 1.0
    else NaN

  private def cmp(l: Type, r: Type): Double =
    if (l == r) 0.0
    else
      (l, r) match {
        case (x: ScalarType, y: ScalarType) => ScalarType.scalarOrder.partialCompare(x, y)
        case (LiteralType(xs, _), y: ScalarType) if xs == Set(y) => 0.0
        case (LiteralType(xs, _), y: ScalarType) if xs(y) => -1.0
        case (x: ScalarType, LiteralType(ys, _)) if ys == Set(x) => 0.0
        case (x: ScalarType, LiteralType(ys, _)) if ys(x) => 1.0
        case (x: ArrayType, y: ArrayType) => cmp(x.element, y.element)
        case (x: ArrayType, y: StreamType) => cmp(x.element, y.element)
        case (x: StreamType, y: StreamType) => cmp(x.element, y.element)
        case (ProductType(_, xFields), ProductType(_, yFields)) =>
          cmpProd(xFields, yFields)
        case (l: ArrowType, r: ArrowType) =>
          val argL = l.args
          val resL = l.res
          val argR = r.args
          val resR = r.res
          val cmpTypes = cmpTypesList(argR, argL)
          val cmpRes =
            if (resL == resR) 0.0
            else (resL, resR).mapN(cmp).getOrElse(NaN)

          if (cmpTypes >= 0 && cmpRes >= 0) 1.0
          else if (cmpTypes <= 0 && cmpRes <= 0) -1.0
          else NaN

        case _ =>
          Double.NaN
      }

  implicit lazy val typesPartialOrder: PartialOrder[Type] = PartialOrder.from(cmp)
}
