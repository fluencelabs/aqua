package aqua.raw.value

import aqua.errors.Errors.internalError
import aqua.types.*
import aqua.types.Type.*
import cats.{Eq, Functor}
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import cats.syntax.option.*
import cats.syntax.functor.*

sealed trait ValueRaw {

  def baseType: Type

  def `type`: Type = baseType

  def renameVars(map: Map[String, String]): ValueRaw

  /**
   * Apply function to all values in the tree
   */
  final def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(mapValues(_.map(f)))

  /**
   * Apply function to values in this value
   */
  def mapValues(f: ValueRaw => ValueRaw): ValueRaw

  def varNames: Set[String]
}

object ValueRaw {
  val InitPeerId: LiteralRaw = LiteralRaw("%init_peer_id%", ScalarType.string)
  val ParticleTtl: LiteralRaw = LiteralRaw("%ttl%", ScalarType.u32)
  val ParticleTimestamp: LiteralRaw = LiteralRaw("%timestamp%", ScalarType.u64)

  val Nil: LiteralRaw = LiteralRaw("[]", StreamType(BottomType))

  /**
   * Type of error value
   */
  val errorType = StructType(
    "Error",
    NonEmptyMap.of(
      /**
       * `message` and `error_code` are always present
       * For no-error state `message = ""` and `error_code = 0`
       */
      "message" -> ScalarType.string,
      "error_code" -> ScalarType.i64,
      /**
       * Instruction that caused error
       * For no-error state accessing this leads to error
       */
      "instruction" -> ScalarType.string,
      /**
       * Peer id that caused error
       * Only set for `call` and `canon` instructions
       * For no-error state accessing this leads to error
       */
      "peer_id" -> ScalarType.string
    )
  )

  val error: VarRaw = VarRaw(
    ":error:",
    errorType
  )

  type ApplyRaw = ApplyPropertyRaw | CallArrowRaw | CollectionRaw | StreamRaw | ApplyBinaryOpRaw |
    ApplyUnaryOpRaw

  extension (v: ValueRaw) {
    def add(a: ValueRaw): ValueRaw = ApplyBinaryOpRaw.Add(v, a)

    def increment: ValueRaw = ApplyBinaryOpRaw.Add(v, LiteralRaw.number(1))
  }
}

case class ApplyPropertyRaw(value: ValueRaw, property: PropertyRaw) extends ValueRaw {
  override def baseType: Type = value.baseType

  override def `type`: Type = property.`type`

  override def renameVars(map: Map[String, String]): ValueRaw =
    ApplyPropertyRaw(value.renameVars(map), property.renameVars(map))

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    ApplyPropertyRaw(f(value), property.map(f))

  override def toString: String = s"$value.$property"

  def unwind: (ValueRaw, Chain[PropertyRaw]) = value match {
    case alr: ApplyPropertyRaw =>
      val (v, i) = alr.unwind
      (v, i :+ property)
    case _ =>
      (value, Chain.one(property))
  }

  override def varNames: Set[String] = value.varNames ++ property.varNames
}

object ApplyPropertyRaw {

  def fromChain(value: ValueRaw, properties: Chain[PropertyRaw]): ValueRaw =
    properties.foldLeft(value) { case (v, l) =>
      ApplyPropertyRaw(v, l)
    }
}

case class VarRaw(name: String, baseType: Type) extends ValueRaw {

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw = this

  override def renameVars(map: Map[String, String]): VarRaw =
    copy(name = map.getOrElse(name, name))

  override def toString: String = s"var{$name: " + baseType + s"}"

  def withProperty(property: PropertyRaw*): ValueRaw =
    ApplyPropertyRaw.fromChain(this, Chain.fromSeq(property))

  override def varNames: Set[String] = Set(name)
}

case class LiteralRaw(value: String, baseType: Type) extends ValueRaw {
  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw = this

  override def toString: String = s"{$value: ${baseType}}"

  override def varNames: Set[String] = Set.empty

  override def renameVars(map: Map[String, String]): ValueRaw = this
}

object LiteralRaw {
  def quote(value: String): LiteralRaw = LiteralRaw("\"" + value + "\"", LiteralType.string)

  def number(value: Long): LiteralRaw = LiteralRaw(value.toString, LiteralType.forInt(value))

  val Zero: LiteralRaw = number(0)

  val True: LiteralRaw = LiteralRaw("true", LiteralType.bool)
  val False: LiteralRaw = LiteralRaw("false", LiteralType.bool)

  object Integer {

    /*
     * Used to match integer literals in pattern matching
     */
    def unapply(value: ValueRaw): Option[Long] =
      value match {
        case LiteralRaw(value, t) if ScalarType.integer.exists(_.acceptsValueOf(t)) =>
          value.toLongOption
        case _ => none
      }
  }
}

case class StreamRaw(values: List[ValueRaw], streamName: String, streamType: StreamType) extends ValueRaw {
  lazy val elementType: DataType = streamType.element

  override lazy val baseType: Type = streamType

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw = {
    val (vals, types) = CollectionRaw.mapCollection(f, values)

    val element = CollectionType.elementTypeOf(types)

    copy(
      values = vals,
      streamType = streamType.withElement(element)
    )
  }

  override def varNames: Set[String] = (values.flatMap(_.varNames) :+ streamName).toSet

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(values = values.map(_.renameVars(map)), streamName = map.getOrElse(streamName, streamName))
}

case class CollectionRaw(
  values: NonEmptyList[ValueRaw],
  collectionType: ImmutableCollectionType
) extends ValueRaw {

  lazy val elementType: DataType = collectionType.element

  override lazy val baseType: Type = collectionType

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw = {
    val (vals, types) = CollectionRaw.mapCollection(f, values)

    val element = CollectionType.elementTypeOf(types.toList)

    copy(
      values = vals,
      collectionType = collectionType.withElement(element)
    )
  }

  override def varNames: Set[String] = values.toList.flatMap(_.varNames).toSet

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(values = values.map(_.renameVars(map)))
}

object CollectionRaw {
  def mapCollection[F[_]: Functor](f: ValueRaw => ValueRaw, values: F[ValueRaw]): (F[ValueRaw], F[CollectibleType]) = {
    val vals = values.map(f)
    val types = vals.map(_.`type` match {
      case ct: CollectibleType => ct
      case t => internalError(s"Non-collection type in collection: ${t}")
    })

    (vals, types)
  }
}

case class MakeStructRaw(fields: NonEmptyMap[String, ValueRaw], structType: StructType)
    extends ValueRaw {

  override def baseType: Type = structType

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    copy(fields = fields.map(f))

  override def varNames: Set[String] = {
    fields.toSortedMap.values.flatMap(_.varNames).toSet
  }

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(fields = fields.map(_.renameVars(map)))
}

case class AbilityRaw(fieldsAndArrows: NonEmptyMap[String, ValueRaw], abilityType: AbilityType)
    extends ValueRaw {

  override def baseType: Type = abilityType

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    copy(fieldsAndArrows = fieldsAndArrows.map(f))

  override def varNames: Set[String] = {
    fieldsAndArrows.toSortedMap.values.flatMap(_.varNames).toSet
  }

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(fieldsAndArrows = fieldsAndArrows.map(_.renameVars(map)))
}

case class ApplyBinaryOpRaw(
  op: ApplyBinaryOpRaw.Op,
  left: ValueRaw,
  right: ValueRaw,
  // TODO: Refactor type, get rid of `LiteralType`
  resultType: ScalarType | LiteralType
) extends ValueRaw {

  override val baseType: Type = resultType

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    copy(left = f(left), right = f(right))

  override def varNames: Set[String] = left.varNames ++ right.varNames

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(left = left.renameVars(map), right = right.renameVars(map))

  override def toString(): String =
    s"(${left} ${op} ${right}) :: ${resultType}"
}

object ApplyBinaryOpRaw {

  enum Op {
    case And, Or
    case Eq, Neq
    case Lt, Lte, Gt, Gte
    case Add, Sub, Mul, FMul, Div, Pow, Rem
  }

  object Op {

    type Bool = And.type | Or.type

    type Eq = Eq.type | Neq.type

    type Cmp = Lt.type | Lte.type | Gt.type | Gte.type

    type Math = Add.type | Sub.type | Mul.type | FMul.type | Div.type | Pow.type | Rem.type
  }

  object Add {

    def apply(left: ValueRaw, right: ValueRaw): ValueRaw =
      ApplyBinaryOpRaw(
        Op.Add,
        left,
        right,
        ScalarType.resolveMathOpType(left.`type`, right.`type`).`type`
      )

    def unapply(value: ValueRaw): Option[(ValueRaw, ValueRaw)] =
      value match {
        case ApplyBinaryOpRaw(Op.Add, left, right, _) =>
          (left, right).some
        case _ => none
      }
  }

  object Sub {

    def apply(left: ValueRaw, right: ValueRaw): ValueRaw =
      ApplyBinaryOpRaw(
        Op.Sub,
        left,
        right,
        ScalarType.resolveMathOpType(left.`type`, right.`type`).`type`
      )

    def unapply(value: ValueRaw): Option[(ValueRaw, ValueRaw)] =
      value match {
        case ApplyBinaryOpRaw(Op.Sub, left, right, _) =>
          (left, right).some
        case _ => none
      }
  }
}

case class ApplyUnaryOpRaw(
  op: ApplyUnaryOpRaw.Op,
  value: ValueRaw
) extends ValueRaw {

  // Only boolean operations are supported for now
  override def baseType: Type = ScalarType.bool

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    copy(value = f(value))

  override def varNames: Set[String] = value.varNames

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(value = value.renameVars(map))
}

object ApplyUnaryOpRaw {

  enum Op {
    case Not
  }
}

case class CallArrowRaw(
  // TODO: ability should hold a type, not name
  ability: Option[String],
  name: String,
  arguments: List[ValueRaw],
  baseType: ArrowType
) extends ValueRaw {
  override def `type`: Type = baseType.codomain.headOption.getOrElse(baseType)

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    copy(arguments = arguments.map(f))

  override def varNames: Set[String] = name.some
    .filterNot(_ => ability.isDefined)
    .toSet ++ arguments.flatMap(_.varNames).toSet

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(
      name = map
        .get(name)
        // Rename only if it is **not** an ability call, see [bug LNG-199]
        .filterNot(_ => ability.isDefined)
        .getOrElse(name)
    )

  override def toString: String =
    s"${ability.fold("")(a => s"$a.")}$name(${arguments.mkString(",")}) :: $baseType)"
}

object CallArrowRaw {

  def func(
    funcName: String,
    baseType: ArrowType,
    arguments: List[ValueRaw] = Nil
  ): CallArrowRaw = CallArrowRaw(
    ability = None,
    name = funcName,
    arguments = arguments,
    baseType = baseType
  )

  def ability(
    abilityName: String,
    funcName: String,
    baseType: ArrowType,
    arguments: List[ValueRaw] = Nil
  ): CallArrowRaw = CallArrowRaw(
    ability = None,
    name = AbilityType.fullName(abilityName, funcName),
    arguments = arguments,
    baseType = baseType
  )

}

/**
 * WARNING: This class is internal and is used to generate code.
 * Calls to services in aqua code are represented as [[CallArrowRaw]]
 * and resolved through ability resolution.
 *
 * @param serviceId service id
 * @param fnName service method name
 * @param baseType type of the service method
 * @param arguments call arguments
 */
case class CallServiceRaw(
  serviceId: ValueRaw,
  fnName: String,
  baseType: ArrowType,
  arguments: List[ValueRaw]
) extends ValueRaw {
  override def `type`: Type = baseType.codomain.headOption.getOrElse(baseType)

  override def mapValues(f: ValueRaw => ValueRaw): ValueRaw =
    copy(
      serviceId = f(serviceId),
      arguments = arguments.map(f)
    )

  override def varNames: Set[String] =
    arguments
      .flatMap(_.varNames)
      .toSet ++ serviceId.varNames

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(
      serviceId = serviceId.renameVars(map),
      arguments = arguments.map(_.renameVars(map))
    )

  override def toString: String =
    s"call (${serviceId}) $fnName(${arguments.mkString(",")}) :: $baseType)"
}
