package aqua.types

import cats.Eval
import cats.PartialOrder
import cats.data.NonEmptyMap
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.partialOrder.*
import cats.syntax.traverse.*

sealed trait Type {

  def acceptsValueOf(incoming: Type): Boolean =
    this >= incoming

  def isInhabited: Boolean = true

  infix def `∩`(other: Type): Type = intersectBottom(other)

  private final def intersectTop(other: Type): Type =
    IntersectTypes.top.combine(this, other)

  private final def intersectBottom(other: Type): Type =
    IntersectTypes.bottom.combine(this, other)

  infix def `∪`(other: Type): Type = uniteTop(other)

  private final def uniteTop(other: Type): Type =
    UniteTypes.top.combine(this, other)

  private final def uniteBottom(other: Type): Type =
    UniteTypes.bottom.combine(this, other)

  def properties: Map[String, Type] = Map.empty

  /**
   * Use for printing purposes only
   * Ideally should be in sync with [[AirGen.varNameToString]]
   */
  def airPrefix: String = this match {
    case _: StreamType => "$"
    case _: CanonStreamType => "#"
    case _ => ""
  }
}

// Product is a list of (optionally labelled) types
sealed trait ProductType extends Type {
  def isEmpty: Boolean = this == NilType

  def length: Int

  def uncons: Option[(Type, ProductType)] = this match {
    case ConsType(t, pt) => Some(t -> pt)
    case _ => None
  }

  def headOption: Option[Type] = this match {
    case ConsType(t, _) => Some(t)
    case _ => None
  }

  lazy val toList: List[Type] = this match {
    case ConsType(t, pt) => t :: pt.toList
    case _ => Nil
  }

  /**
   * Converts product type to a list of types, labelling each of them with a string
   * Label is either got from the types with labels, or from the given prefix and index of a type.
   *
   * @param prefix Prefix to generate a missing label
   * @param index  Index to ensure generated labels are unique
   * @return
   */
  def toLabelledList(prefix: String = "arg", index: Int = 0): List[(String, Type)] = this match {
    case LabeledConsType(label, t, pt) => (label -> t) :: pt.toLabelledList(prefix, index + 1)
    case UnlabeledConsType(t, pt) => (s"$prefix$index" -> t) :: pt.toLabelledList(prefix, index + 1)
    case _ => Nil
  }

  lazy val labelledData: List[(String, DataType)] = this match {
    case LabeledConsType(label, t: DataType, pt) =>
      (label -> t) :: pt.labelledData
    case ConsType(_, pt) =>
      pt.labelledData
    case _ => Nil
  }

  lazy val labelledStreams: List[(String, StreamType)] = this match {
    case LabeledConsType(label, t: StreamType, pt) =>
      (label -> t) :: pt.labelledStreams
    case ConsType(_, pt) =>
      pt.labelledStreams
    case _ => Nil
  }

}

object ProductType {

  def apply(types: List[Type]): ProductType = types match {
    case h :: t =>
      ConsType.cons(h, ProductType(t))
    case _ => NilType
  }

  def maybeLabelled(types: List[(Option[String], Type)]): ProductType = types match {
    case (Some(l), h) :: t =>
      ConsType.cons(l, h, ProductType.maybeLabelled(t))
    case (None, h) :: t =>
      ConsType.cons(h, ProductType.maybeLabelled(t))
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

  def cons(`type`: Type, tail: ProductType): ConsType = UnlabeledConsType(`type`, tail)

  def cons(label: String, `type`: Type, tail: ProductType): ConsType =
    LabeledConsType(label, `type`, tail)
}

case class LabeledConsType(label: String, `type`: Type, tail: ProductType) extends ConsType {
  override def toString: String = s"($label: " + `type` + s") :: $tail"
}

case class UnlabeledConsType(`type`: Type, tail: ProductType) extends ConsType {
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
  val signed = Set(i8, i16, i32, i64)
  val unsigned = Set(u8, u16, u32, u64)
  val integer = signed ++ unsigned
  val number = float ++ integer
  val all = number ++ Set(bool, string)

  final case class MathOpType(
    `type`: ScalarType | LiteralType,
    overflow: Boolean
  )

  /**
   * Resolve type of math operation
   * on two given types.
   *
   * WARNING: General `Type` is accepted
   * but only integer `ScalarType` and `LiteralType`
   * are actually expected.
   */
  def resolveMathOpType(
    lType: Type,
    rType: Type
  ): MathOpType = {
    val uType = lType `∪` rType
    uType match {
      case t: (ScalarType | LiteralType) => MathOpType(t, false)
      case _ => MathOpType(ScalarType.i64, true)
    }
  }

  /**
   * Check if given type is signed.
   *
   * NOTE: Only integer types are expected.
   * But it is impossible to enforce it.
   */
  def isSignedInteger(t: ScalarType | LiteralType): Boolean =
    t match {
      case st: ScalarType => signed.contains(st)
      /**
       * WARNING: LiteralType.unsigned is signed integer!
       */
      case lt: LiteralType => lt.oneOf.exists(signed.contains)
    }
}

case class LiteralType private (oneOf: Set[ScalarType], name: String) extends DataType {
  override def toString: String = s"$name literal"
}

object LiteralType {
  val float = LiteralType(ScalarType.float, "float")
  val signed = LiteralType(ScalarType.signed, "signed")
  /*
   * Literals without sign could be either signed or unsigned
   * so `ScalarType.integer` is used here
   */
  val unsigned = LiteralType(ScalarType.integer, "unsigned")
  val number = LiteralType(ScalarType.number, "number")
  val bool = LiteralType(Set(ScalarType.bool), "bool")
  val string = LiteralType(Set(ScalarType.string), "string")

  def forInt(n: Long): LiteralType = if (n < 0) signed else unsigned
}

sealed trait CollectionType extends Type {
  def isStream: Boolean

  def element: DataType

  def withElement(t: DataType): CollectionType

  override def properties: Map[String, Type] =
    Map("length" -> ScalarType.u32)
}

case class CanonStreamType(
  override val element: DataType
) extends DataType with CollectionType {

  override val isStream: Boolean = false

  override def toString: String = "#" + element

  override def withElement(t: DataType): CollectionType = copy(element = t)
}

case class ArrayType(
  override val element: DataType
) extends DataType with CollectionType {

  override val isStream: Boolean = false

  override def toString: String = "[]" + element

  override def withElement(t: DataType): CollectionType = copy(element = t)
}

case class OptionType(
  override val element: DataType
) extends DataType with CollectionType {

  override val isStream: Boolean = false

  override def toString: String = "?" + element

  override def withElement(t: DataType): CollectionType = copy(element = t)
}

sealed trait NamedType extends Type {

  def specifier: String
  def name: String

  final def fullName: String = s"$specifier $name"

  def fields: NonEmptyMap[String, Type]

  /**
   * Get all arrows defined in this type and its sub-abilities.
   * Paths to arrows are returned **without** type name
   * to allow renaming on call site.
   */
  lazy val arrows: Map[String, ArrowType] = {
    def getArrowsEval(path: Option[String], nt: NamedType): Eval[List[(String, ArrowType)]] =
      nt.fields.toNel.toList.flatTraverse {
        // sub-arrows could be in abilities or services
        case (innerName, innerType: (ServiceType | AbilityType)) =>
          val newPath = path.fold(innerName)(AbilityType.fullName(_, innerName))
          getArrowsEval(newPath.some, innerType)
        case (aName, aType: ArrowType) =>
          val newPath = path.fold(aName)(AbilityType.fullName(_, aName))
          List(newPath -> aType).pure
        case _ => Nil.pure
      }

    getArrowsEval(None, this).value.toMap
  }

  /**
   * Get all abilities defined in this type and its sub-abilities.
   * Paths to abilities are returned **without** type name
   * to allow renaming on call site.
   */
  lazy val abilities: Map[String, AbilityType] = {
    def getAbilitiesEval(
      path: Option[String],
      nt: NamedType
    ): Eval[List[(String, AbilityType)]] =
      nt.fields.toNel.toList.flatTraverse {
        // sub-abilities could be only in abilities
        case (abName, abType: AbilityType) =>
          val fullName = path.fold(abName)(AbilityType.fullName(_, abName))
          getAbilitiesEval(fullName.some, abType).map(
            (fullName -> abType) :: _
          )
        case _ => Nil.pure
      }

    getAbilitiesEval(None, this).value.toMap
  }

  /**
   * Get all variables defined in this type and its sub-abilities.
   * Paths to variables are returned **without** type name
   * to allow renaming on call site.
   */
  lazy val variables: Map[String, DataType] = {
    def getVariablesEval(
      path: Option[String],
      nt: NamedType
    ): Eval[List[(String, DataType)]] =
      nt.fields.toNel.toList.flatTraverse {
        // sub-variables could be only in abilities
        case (abName, abType: AbilityType) =>
          val newPath = path.fold(abName)(AbilityType.fullName(_, abName))
          getVariablesEval(newPath.some, abType)
        case (dName, dType: DataType) =>
          val newPath = path.fold(dName)(AbilityType.fullName(_, dName))
          List(newPath -> dType).pure
        case _ => Nil.pure
      }

    getVariablesEval(None, this).value.toMap
  }
}

// Struct is an unordered collection of labelled types
// TODO: Make fields type `DataType`
case class StructType(name: String, fields: NonEmptyMap[String, Type])
    extends DataType with NamedType {

  override val specifier: String = "data"

  override def toString: String =
    s"$fullName{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

sealed trait MutableStreamType extends Type with CollectionType

case class StreamMapType(override val element: DataType) extends MutableStreamType {

  override val isStream: Boolean = true

  override def withElement(t: DataType): CollectionType = copy(element = t)

  override def toString: String = s"%$element"
}

object StreamMapType {
  def top(): StreamMapType = StreamMapType(TopType)
}

case class StreamType(override val element: DataType) extends MutableStreamType {

  override val isStream: Boolean = true

  override def toString: String = s"*$element"

  override def withElement(t: DataType): CollectionType = copy(element = t)
}

case class ServiceType(name: String, fields: NonEmptyMap[String, ArrowType]) extends NamedType {

  override val specifier: String = "service"

  override def toString: String =
    s"$fullName{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

// Ability is an unordered collection of labelled types and arrows
case class AbilityType(name: String, fields: NonEmptyMap[String, Type]) extends NamedType {

  override val specifier: String = "ability"

  override def toString: String =
    s"$fullName{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

object AbilityType {
  def fullName(name: String, field: String) = s"$name.$field"
}

/**
 * ArrowType is a profunctor pointing its domain to codomain.
 * Profunctor means variance: Arrow is contravariant on domain, and variant on codomain.
 * See tests for details.
 *
 * @param domain   Where this Arrow is defined
 * @param codomain Where this Arrow points on
 */
case class ArrowType(domain: ProductType, codomain: ProductType) extends Type {

  lazy val res: Option[Type] = codomain.toList match {
    case Nil => None
    case a :: Nil => Some(a)
    case _ => Some(codomain)
  }

  override def toString: String =
    s"$domain -> $codomain"
}

object Type {

  given PartialOrder[Type] =
    CompareTypes.partialOrder
}
