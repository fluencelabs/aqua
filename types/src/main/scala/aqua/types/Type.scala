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

  def isInhabited: Boolean = true
}

sealed trait ProductType extends Type {
  def isEmpty: Boolean = this == NilType

  def uncons: Option[(Type, ProductType)] = this match {
    case ConsType(t, pt) => Some(t -> pt)
    case _ => None
  }

  def toList: List[Type] = this match {
    case ConsType(t, pt) => t :: pt.toList
    case _ => Nil
  }

  def toLabelledList(prefix: String = "arg", index: Int = 0): List[(String, Type)] = this match {
    case LabelledConsType(label, t, pt) => (label -> t) :: pt.toLabelledList(prefix, index + 1)
    case UnlabelledConsType(t, pt) =>
      (s"$prefix$index" -> t) :: pt.toLabelledList(prefix, index + 1)
    case _ => Nil
  }

  def labelledData: List[(String, DataType)] = this match {
    case LabelledConsType(label, t: DataType, pt) => (label -> t) :: pt.labelledData
    case UnlabelledConsType(_, pt) => pt.labelledData
    case _ => Nil
  }
}

object ProductType {

  def apply(types: List[Type]): ProductType = types match {
    case h :: t =>
      ConsType.cons(h, ProductType(t))
    case _ => NilType
  }

  def labelled(types: List[(String, Type)]): ProductType = types match {
    case (l, h) :: t =>
      ConsType.cons(l, h, ProductType.labelled(t))
    case _ => NilType
  }
}

sealed trait ConsType extends ProductType {
  def `type`: Type
  def tail: ProductType
}

object ConsType {
  def unapply(cons: ConsType): Option[(Type, ProductType)] = Some(cons.`type` -> cons.tail)
  def cons(`type`: Type, tail: ProductType): ConsType = UnlabelledConsType(`type`, tail)

  def cons(label: String, `type`: Type, tail: ProductType): ConsType =
    LabelledConsType(label, `type`, tail)
}

case class LabelledConsType(label: String, `type`: Type, tail: ProductType) extends ConsType {
  override def toString: String = s"($label: " + `type` + s" :: $tail"
}

case class UnlabelledConsType(`type`: Type, tail: ProductType) extends ConsType {
  override def toString: String = `type` + s" :: $tail"
}

object NilType extends ProductType {
  override def toString: String = "∅"

  override def isInhabited: Boolean = false
}

sealed trait DataType extends Type

case object TopType extends DataType {
  override def toString: String = "⊤"
}

case object BottomType extends DataType {
  override def toString: String = "⊥"

  override def isInhabited: Boolean = false
}

case class ScalarType private (name: String) extends DataType {
  override def toString: String = name
}

object ScalarType {
  // https://github.com/fluencelabs/interface-types/blob/master/crates/it-types/src/values.rs
  val u8 = ScalarType("u8")
  val u16 = ScalarType("u16")
  val u32 = ScalarType("u32")
  val u64 = ScalarType("u64")

  val i8 = ScalarType("i8")
  val i16 = ScalarType("i16")
  val i32 = ScalarType("i32")
  val i64 = ScalarType("i64")

  val f32 = ScalarType("f32")
  val f64 = ScalarType("f64")

  val bool = ScalarType("bool")
  val string = ScalarType("string")

  val float = Set(f32, f64)
  val signed = float ++ Set(i8, i16, i32, i64)
  val number = signed ++ Set(u8, u16, u32, u64)
  val all = number ++ Set(bool, string)

  private def isLessThen(a: ScalarType, b: ScalarType): Boolean = (a, b) match {
    // Signed numbers
    case (`i32` | `i16` | `i8`, `i64`) => true
    case (`i16` | `i8`, `i32`) => true
    case (`i8`, `i16`) => true

    // Unsigned numbers -- can fit into larger signed ones too
    case (`u32` | `u16` | `u8`, `u64` | `i64`) => true
    case (`u16` | `u8`, `u32` | `i32`) => true
    case (`u8`, `u16` | `i16`) => true

    // Floats
    case (`f32`, `f64`) => true

    case (`i8` | `i16` | `u8` | `u16`, `f32` | `f64`) => true
    case (`i32` | `u32`, `f64`) => true

    case _ => false
  }

  val scalarOrder: PartialOrder[ScalarType] =
    PartialOrder.from {
      case (a, b) if a == b => 0.0
      case (a, b) if isLessThen(a, b) => -1.0
      case (a, b) if isLessThen(b, a) => 1.0
      case _ => Double.NaN
    }
}

case class LiteralType private (oneOf: Set[ScalarType], name: String) extends DataType {
  override def toString: String = s"literal($name)"
}

object LiteralType {
  val float = LiteralType(ScalarType.float, "float")
  val signed = LiteralType(ScalarType.signed, "signed")
  val number = LiteralType(ScalarType.number, "number")
  val bool = LiteralType(Set(ScalarType.bool), "bool")
  val string = LiteralType(Set(ScalarType.string), "string")
}

sealed trait BoxType extends DataType {
  def element: Type
}

case class ArrayType(element: Type) extends BoxType {
  override def toString: String = "[]" + element
}

case class OptionType(element: Type) extends BoxType {
  override def toString: String = "?" + element
}

case class StructType(name: String, fields: NonEmptyMap[String, Type]) extends DataType {

  override def toString: String =
    s"$name{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

case class ArrowType(domain: ProductType, codomain: ProductType) extends Type {

  def args: List[Type] = domain.toList
  def res: Option[Type] = codomain.uncons.map(_._1)

  def acceptsAsArguments(valueTypes: List[Type]): Boolean =
    (args.length == valueTypes.length) && args
      .zip(valueTypes)
      .forall(av => av._1.acceptsValueOf(av._2))

  override def toString: String =
    s"$domain -> $codomain"
}

case class StreamType(element: Type) extends BoxType

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
    ) 1.0
    else if (
      rf.keys.forall(lf.contains) && cmpTypesList(
        lf.toSortedMap.view.filterKeys(rf.keys.contains).toList.map(_._2),
        rf.toSortedMap.toList.map(_._2)
      ) == 1.0
    ) -1.0
    else NaN

  private def cmp(l: Type, r: Type): Double =
    if (l == r) 0.0
    else
      (l, r) match {
        case (TopType, _) | (_, BottomType) => 1.0
        case (BottomType, _) | (_, TopType) => -1.0
        case (x: ScalarType, y: ScalarType) => ScalarType.scalarOrder.partialCompare(x, y)
        case (LiteralType(xs, _), y: ScalarType) if xs == Set(y) => 0.0
        case (LiteralType(xs, _), y: ScalarType) if xs(y) => -1.0
        case (x: ScalarType, LiteralType(ys, _)) if ys == Set(x) => 0.0
        case (x: ScalarType, LiteralType(ys, _)) if ys(x) => 1.0
        case (x: ArrayType, y: ArrayType) => cmp(x.element, y.element)
        case (x: ArrayType, y: StreamType) => cmp(x.element, y.element)
        case (x: ArrayType, y: OptionType) => cmp(x.element, y.element)
        case (x: OptionType, y: StreamType) => cmp(x.element, y.element)
        case (x: OptionType, y: ArrayType) => cmp(x.element, y.element)
        case (x: StreamType, y: StreamType) => cmp(x.element, y.element)
        case (StructType(_, xFields), StructType(_, yFields)) =>
          cmpProd(xFields, yFields)
        case (_: ConsType, NilType) => -1.0
        case (NilType, _: ConsType) => 1.0
        case (ConsType(lhead, ltail), ConsType(rhead, rtail)) =>
          // If any is not Cons, than it's Bottom and already handled
          val headCmp = cmp(lhead, rhead)
          if (headCmp.isNaN) NaN
          else {
            val tailCmp = cmp(ltail, rtail)
            // If one is >, and another eq, it's >, and vice versa
            if (headCmp >= 0 && tailCmp >= 0) 1.0
            else if (headCmp <= 0 && tailCmp <= 0) -1.0
            else NaN
          }

        case (ArrowType(ldom, lcodom), ArrowType(rdom, rcodom)) =>
          val cmpDom = cmp(ldom, rdom)
          val cmpCodom = cmp(lcodom, rcodom)

          if (cmpDom >= 0 && cmpCodom >= 0) -1.0
          else if (cmpDom <= 0 && cmpCodom <= 0) 1.0
          else NaN

        case _ =>
          Double.NaN
      }

  implicit lazy val typesPartialOrder: PartialOrder[Type] = PartialOrder.from(cmp)
}
