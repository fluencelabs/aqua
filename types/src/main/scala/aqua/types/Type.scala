package aqua.types

import cats.PartialOrder
import cats.data.NonEmptyMap

sealed trait Type {

  def acceptsValueOf(incoming: Type): Boolean = {
    import Type.typesPartialOrder
    import cats.syntax.partialOrder._
    this >= incoming
  }

  def isInhabited: Boolean = true
}

// Product is a list of (optionally labelled) types
sealed trait ProductType extends Type {
  def isEmpty: Boolean = this == NilType

  def length: Int

  def uncons: Option[(Type, ProductType)] = this match {
    case ConsType(t, pt) => Some(t -> pt)
    case _ => None
  }

  lazy val toList: List[Type] = this match {
    case ConsType(t, pt) => t :: pt.toList
    case _ => Nil
  }

  /**
   * Converts product type to a list of types, labelling each of them with a string
   * Label is either got from the types with labels, or from the given prefix and index of a type.
   * @param prefix Prefix to generate a missing label
   * @param index Index to ensure generated labels are unique
   * @return
   */
  def toLabelledList(prefix: String = "arg", index: Int = 0): List[(String, Type)] = this match {
    case LabelledConsType(label, t, pt) => (label -> t) :: pt.toLabelledList(prefix, index + 1)
    case UnlabelledConsType(t, pt) =>
      (s"$prefix$index" -> t) :: pt.toLabelledList(prefix, index + 1)
    case _ => Nil
  }

  lazy val labelledData: List[(String, DataType)] = this match {
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

/**
 * ConsType adds a type to the ProductType, and delegates all the others to tail
 * Corresponds to Cons (::) in the List
 */
sealed trait ConsType extends ProductType {
  def `type`: Type
  def tail: ProductType

  override def length: Int = 1 + tail.length
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
  override def toString: String = `type`.toString + s" :: $tail"
}

object NilType extends ProductType {
  override def toString: String = "∅"

  override def isInhabited: Boolean = false

  override def length: Int = 0
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

// Struct is an unordered collection of labelled types
case class StructType(name: String, fields: NonEmptyMap[String, Type]) extends DataType {

  override def toString: String =
    s"$name{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

case class ArrowType(domain: ProductType, codomain: ProductType) extends Type {

  @deprecated(
    "Use .domain to get arguments, add .args helper to the typed object, if needed",
    "5.08.2021"
  )
  def args: List[Type] = domain.toList

  @deprecated(
    "Use .codomain to get results, add .res helper to the typed object, if needed; consider multi-value return",
    "5.08.2021"
  )
  def res: Option[Type] = codomain.uncons.map(_._1)

  @deprecated(
    "Replace with this function's body",
    "5.08.2021"
  )
  def acceptsAsArguments(valueTypes: List[Type]): Boolean =
    domain.acceptsValueOf(ProductType(valueTypes))

  override def toString: String =
    s"$domain -> $codomain"
}

case class StreamType(element: Type) extends BoxType

object Type {

  implicit lazy val typesPartialOrder: PartialOrder[Type] =
    CompareTypes.partialOrder
}
