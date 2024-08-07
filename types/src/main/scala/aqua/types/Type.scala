/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.types

import aqua.errors.Errors.internalError
import aqua.types.*
import aqua.types.Type.*

import cats.data.NonEmptyMap
import cats.syntax.applicative.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.partialOrder.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.{Eval, Foldable, Functor, PartialOrder, Show, Traverse}
import scala.collection.immutable.SortedMap

sealed trait Type {

  def acceptsValueOf(incoming: Type): Boolean =
    this >= incoming

  def isInhabited: Boolean = true

  infix def `∩`[T <: Type](other: T): Type = intersectBottom(other)

  private final def intersectTop(other: Type): Type =
    IntersectTypes.top.combine(this, other)

  private final def intersectBottom(other: Type): Type =
    IntersectTypes.bottom.combine(this, other)

  infix def `∪`[T <: Type](other: T): Type = uniteTop(other)

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
    case _: CanonStreamType => "#$"
    case _: StreamMapType => "%"
    case _: CanonStreamMapType => "#%"
    case _ => ""
  }
}

// Product is a list of (optionally labelled) types
sealed trait ProductType extends Type {
  def isEmpty: Boolean = this == NilType

  def length: Int

  def map(f: Type => Type): ProductType

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
  def map(f: Type => Type): ProductType = copy(`type` = f(`type`), tail = tail.map(f))

  override def toString: String = s"($label: " + `type` + s") :: $tail"
}

case class UnlabeledConsType(`type`: Type, tail: ProductType) extends ConsType {
  def map(f: Type => Type): ProductType = copy(`type` = f(`type`), tail = tail.map(f))

  override def toString: String = `type`.toString + s" :: $tail"
}

object NilType extends ProductType {
  def map(f: Type => Type): ProductType = this

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

object CollectionType {

  def elementTypeOf[F[_]: Foldable: Functor](types: F[CollectibleType]): DataType =
    types
      .map[Type] {
        case StreamType(el) => ArrayType(el)
        case dt: DataType => dt
      }
      .reduceLeftOption(_ `∩` _)
      .map {
        // In case we mix values of uncomparable types, intersection returns bottom, meaning "uninhabited type".
        // But we want to get to TopType instead: this would mean that intersection is empty, and you cannot
        // make any decision about the structure of type, but can push anything inside
        case BottomType => TopType
        case dt: DataType => dt
        case t =>
          internalError(
            s"Expected data type from " +
              s"intersection of ${types.foldLeft("") { case (l, r) => l + ", " + r }}; " +
              s"got $t"
          )
      }
      .getOrElse(BottomType)

}

sealed trait ImmutableCollectionType extends CollectionType with DataType {
  def withElement(t: DataType): ImmutableCollectionType
}

sealed trait MutableStreamType extends CollectionType {
  def toCanon: ImmutableCollectionType
}

case class CanonStreamType(
  override val element: DataType
) extends ImmutableCollectionType {

  override val isStream: Boolean = false

  override def toString: String = "#$" + element

  override def withElement(t: DataType): ImmutableCollectionType = copy(element = t)
}

case class CanonStreamMapType(
  override val element: DataType
) extends ImmutableCollectionType {

  override val isStream: Boolean = false

  override def toString: String = "#%" + element

  override def withElement(t: DataType): ImmutableCollectionType = copy(element = t)
}

case class ArrayType(
  override val element: DataType
) extends ImmutableCollectionType {

  override val isStream: Boolean = false

  override def toString: String = "[]" + element

  override def withElement(t: DataType): ImmutableCollectionType = copy(element = t)
}

case class OptionType(
  override val element: DataType
) extends ImmutableCollectionType {

  override val isStream: Boolean = false

  override def toString: String = "?" + element

  override def withElement(t: DataType): ImmutableCollectionType = copy(element = t)
}

case class StreamMapType(override val element: DataType) extends MutableStreamType {

  import StreamMapType.Func
  import StreamMapType.Func.*

  override val isStream: Boolean = true

  override def withElement(t: DataType): MutableStreamType = copy(element = t)

  override def toString: String = s"%$element"

  def getFunc(f: Func): ArrowType = {
    val (args, rets) = f match {
      case Get =>
        (ScalarType.string :: Nil) -> (ArrayType(element) :: Nil)
      case GetStream =>
        (ScalarType.string :: Nil) -> (StreamType(element) :: Nil)
      case Keys =>
        Nil -> (ArrayType(ScalarType.string) :: Nil)
      case KeysStream =>
        Nil -> (StreamType(ScalarType.string) :: Nil)
      case Contains =>
        (ScalarType.string :: Nil) -> (ScalarType.bool :: Nil)
    }

    ArrowType(ProductType(args), ProductType(rets))
  }

  def funcByString(s: String): Option[ArrowType] = {
    StreamMapType.funcByString(s).map(getFunc)
  }

  def iterType(name: String): StructType =
    StructType(name, NonEmptyMap.of("key" -> ScalarType.string, "value" -> element))

  def toCanon: ImmutableCollectionType = CanonStreamMapType(element)

  def elementProduct: ProductType = ProductType(ScalarType.string :: element :: Nil)
}

object StreamMapType {

  enum Func(val name: String) {
    case Get extends Func("get")
    case GetStream extends Func("getStream")
    case Keys extends Func("keys")
    case KeysStream extends Func("keysStream")
    case Contains extends Func("contains")
  }

  def funcByString(s: String): Option[Func] =
    Func.values.find(_.name == s)

  lazy val allFuncs: List[Func] = Func.values.toList

  def top(): StreamMapType = StreamMapType(TopType)
}

case class StreamType(override val element: DataType) extends MutableStreamType {

  override val isStream: Boolean = true

  override def toString: String = s"*$element"

  override def withElement(t: DataType): StreamType = copy(element = t)

  def toCanon: ImmutableCollectionType = CanonStreamType(element)
}

sealed trait NamedType extends Type {

  def specifier: String
  def name: String

  final def fullName: String = s"$specifier $name"

  def fields: NonEmptyMap[String, Type]

  def arrowFields: Map[String, ArrowType] =
    fields.toSortedMap.collect { case (name, at: ArrowType) =>
      name -> at
    }

  /**
   * Get all fields defined in this type and its fields of named type.
   * Paths to fields are returned **without** type name
   * to allow renaming on call site.
   */
  final def allFields: NonEmptyMap[String, Type] = {
    def allEval(path: Option[String], nt: NamedType): Eval[List[(String, Type)]] = {
      val qualified = (name: String) => path.fold(name)(AbilityType.fullName(_, name))
      val fieldsList = nt.fields.toNel.toList
      val currentFields = fieldsList.map { case (name, t) =>
        qualified(name) -> t
      }
      fieldsList.flatTraverse {
        case (name, t: NamedType) =>
          allEval(qualified(name).some, t)
        case _ => Eval.now(Nil)
      }.map(currentFields ++ _)
    }

    allEval(none, this)
      .map(l =>
        /**
         * As fields are `NonEmptyMap`, this
         * operation should be safe
         */
        NonEmptyMap.fromMapUnsafe(SortedMap.from(l))
      )
      .value
  }

  /**
   * Get all arrows defined in this type and its sub-abilities.
   * Paths to arrows are returned **without** type name
   * to allow renaming on call site.
   */
  lazy val arrows: Map[String, ArrowType] =
    allFields.toSortedMap.collect { case (name, at: ArrowType) =>
      name -> at
    }

  /**
   * Get all abilities defined in this type and its sub-abilities.
   * Paths to abilities are returned **without** type name
   * to allow renaming on call site.
   */
  lazy val abilities: Map[String, AbilityType] =
    allFields.toSortedMap.collect { case (name, at: AbilityType) =>
      name -> at
    }

  /**
   * Get all variables defined in this type and its sub-abilities.
   * Paths to variables are returned **without** type name
   * to allow renaming on call site.
   */
  lazy val variables: Map[String, DataType] =
    allFields.toSortedMap.collect { case (name, at: DataType) =>
      name -> at
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

/**
 * This type unites types that work as abilities,
 * namely `ServiceType` and `AbilityType`
 */
sealed trait GeneralAbilityType extends NamedType

case class ServiceType(
  name: String,
  fields: NonEmptyMap[String, ArrowType]
) extends GeneralAbilityType {

  override val specifier: String = "service"

  override def toString: String =
    s"$fullName{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

// Ability is an unordered collection of labelled types and arrows
case class AbilityType(
  name: String,
  fields: NonEmptyMap[String, Type]
) extends GeneralAbilityType {

  override val specifier: String = "ability"

  override def toString: String =
    s"$fullName{${fields.map(_.toString).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")}}"
}

object AbilityType {
  def fullName(name: String, field: String) = s"$name.$field"

  def renames(at: NamedType)(
    name: String,
    newName: String
  ): Map[String, String] =
    at.allFields.keys.toList
      .map(path =>
        val fullName = AbilityType.fullName(name, path)
        val newFullName = AbilityType.fullName(newName, path)
        fullName -> newFullName
      )
      .toMap
      .updated(name, newName)
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

  /**
   * `StreamType` is collectible with canonicalization.
   * Note: until aqua type system has immutable maps,
   * they are not collectible
   */
  type CollectibleType = DataType | StreamType

  def isStreamType(t: Type): Boolean =
    t match {
      case _: MutableStreamType => true
      case _ => false
    }

  def isStreamMapType(t: Type): Boolean =
    t match {
      case _: StreamMapType => true
      case _ => false
    }

  given PartialOrder[Type] =
    CompareTypes.partialOrder

  given Show[DataType] = {
    case LiteralType.signed =>
      "i32"
    case LiteralType.unsigned =>
      "u32"
    case LiteralType.number =>
      "u32"
    case LiteralType.float =>
      "f32"
    case LiteralType.string =>
      "string"
    case LiteralType.bool =>
      "bool"
    case t =>
      t.toString
  }

  def addAbilityNameProduct(abName: String, t: ProductType): ProductType =
    t.map(t => addAbilityName(abName, t))

  def addAbilityNameArrow(abName: String, t: ArrowType): ArrowType = {
    t.copy(
      domain = addAbilityNameProduct(abName, t.domain),
      codomain = addAbilityNameProduct(abName, t.codomain)
    )
  }

  def addAbilityNameData(abName: String, dt: DataType): DataType =
    dt match {
      case st @ StructType(name, fields) =>
        st.copy(
          name = AbilityType.fullName(abName, name),
          fields = fields.map(Type.addAbilityName(abName, _))
        )
      case ot @ OptionType(el) => ot.copy(element = addAbilityNameData(abName, el))
      case at @ ArrayType(el) => at.copy(element = addAbilityNameData(abName, el))
      case t => t
    }

  def addAbilityNameService(abName: String, t: ServiceType): ServiceType = {
    t.copy(
      name = AbilityType.fullName(abName, t.name),
      fields = t.fields.map(Type.addAbilityNameArrow(abName, _))
    )
  }

  // Add ability name to type names
  def addAbilityName(abName: String, t: Type): Type = {
    t match {
      case at @ AbilityType(name, fields) =>
        at.copy(
          name = AbilityType.fullName(abName, name),
          fields = fields.map(Type.addAbilityName(abName, _))
        )
      case st: ServiceType =>
        addAbilityNameService(abName, st)

      case at: ArrowType =>
        addAbilityNameArrow(abName, at)
      case st @ CanonStreamType(el) => st.copy(element = addAbilityNameData(abName, el))
      case st @ StreamType(el) => st.copy(element = addAbilityNameData(abName, el))
      case smt @ StreamMapType(el) => smt.copy(element = addAbilityNameData(abName, el))
      case pt: ProductType => addAbilityNameProduct(abName, pt)
      case t: DataType => addAbilityNameData(abName, t)
    }
  }

  // pretty print for Type
  given Show[Type] = {
    case ArrayType(el) =>
      s"[]${el.show}"
    case OptionType(el) =>
      s"?${el.show}"
    case StreamType(el) =>
      s"*${el.show}"
    case ArrowType(domain, codomain) =>
      val domainStr = domain match {
        case _: LabeledConsType =>
          domain.toLabelledList().map { case (s, t) => s"$s: ${t.show}" }.mkString("(", ", ", ")")
        case _ => domain.toList.mkString("(", ", ", ")")
      }
      val codomainStr = codomain.toList match {
        case Nil => ""
        case l => " -> " + l.mkString(", ")
      }
      domainStr + codomainStr
    case nt: NamedType =>
      s"${nt.fullName}(${nt.fields.map(_.show).toNel.toList.map(kv => kv._1 + ": " + kv._2).mkString(", ")})"
    case t: DataType =>
      t.show
    case t =>
      t.toString
  }
}
