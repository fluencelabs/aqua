package aqua.raw.value

import aqua.types.*

import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import cats.Eq
import cats.syntax.option.*
import scribe.Logging

sealed trait ValueRaw {

  def baseType: Type

  def `type`: Type = baseType

  def renameVars(map: Map[String, String]): ValueRaw

  def map(f: ValueRaw => ValueRaw): ValueRaw

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

  type ApplyRaw = ApplyGateRaw | ApplyPropertyRaw | CallArrowRaw | CollectionRaw |
    ApplyBinaryOpRaw | ApplyUnaryOpRaw
}

case class ApplyPropertyRaw(value: ValueRaw, property: PropertyRaw) extends ValueRaw {
  override def baseType: Type = value.baseType

  override def `type`: Type = property.`type`

  override def renameVars(map: Map[String, String]): ValueRaw =
    ApplyPropertyRaw(value.renameVars(map), property.renameVars(map))

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(ApplyPropertyRaw(f(value), property.map(_.map(f))))

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

case class ApplyGateRaw(name: String, streamType: StreamType, idx: ValueRaw) extends ValueRaw {
  override def baseType: Type = streamType

  override def `type`: Type = idx.`type`

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(name = map.getOrElse(name, name), idx = idx.renameVars(map))

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(idx = f(idx)))

  override def toString: String = s"gate $name.$idx"

  override def varNames: Set[String] = Set(name) ++ idx.varNames
}

case class VarRaw(name: String, baseType: Type) extends ValueRaw {

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(this)

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(name = map.getOrElse(name, name))

  override def toString: String = s"var{$name: " + baseType + s"}"

  def withProperty(property: PropertyRaw*): ValueRaw =
    ApplyPropertyRaw.fromChain(this, Chain.fromSeq(property))

  override def varNames: Set[String] = Set(name)
}

case class LiteralRaw(value: String, baseType: Type) extends ValueRaw {
  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(this)

  override def toString: String = s"{$value: ${baseType}}"

  override def varNames: Set[String] = Set.empty

  override def renameVars(map: Map[String, String]): ValueRaw = this
}

object LiteralRaw {
  def quote(value: String): LiteralRaw = LiteralRaw("\"" + value + "\"", LiteralType.string)

  def number(value: Int): LiteralRaw = LiteralRaw(value.toString, LiteralType.forInt(value))

  val Zero: LiteralRaw = number(0)

  val True: LiteralRaw = LiteralRaw("true", LiteralType.bool)
  val False: LiteralRaw = LiteralRaw("false", LiteralType.bool)
}

case class CollectionRaw(values: NonEmptyList[ValueRaw], boxType: BoxType) extends ValueRaw {

  lazy val elementType: Type = boxType.element

  override lazy val baseType: Type = boxType

  override def map(f: ValueRaw => ValueRaw): ValueRaw = {
    val vals = values.map(f)
    val el = vals.map(_.`type`).reduceLeft(_ `∩` _)
    f(copy(vals, boxType.withElement(el)))
  }

  override def varNames: Set[String] = values.toList.flatMap(_.varNames).toSet

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(values = values.map(_.renameVars(map)))
}

case class MakeStructRaw(fields: NonEmptyMap[String, ValueRaw], structType: StructType)
    extends ValueRaw {

  override def baseType: Type = structType

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(copy(fields = fields.map(f)))

  override def varNames: Set[String] = {
    fields.toSortedMap.values.flatMap(_.varNames).toSet
  }

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(fields = fields.map(_.renameVars(map)))
}

case class AbilityRaw(fieldsAndArrows: NonEmptyMap[String, ValueRaw], abilityType: AbilityType)
    extends ValueRaw {

  override def baseType: Type = abilityType

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(fieldsAndArrows = fieldsAndArrows.map(f)))

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
  resultType: Type
) extends ValueRaw {

  override val baseType: Type = resultType

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(left = f(left), right = f(right)))

  override def varNames: Set[String] = left.varNames ++ right.varNames

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(left = left.renameVars(map), right = right.renameVars(map))
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
}

case class ApplyUnaryOpRaw(
  op: ApplyUnaryOpRaw.Op,
  value: ValueRaw
) extends ValueRaw {

  // Only boolean operations are supported for now
  override def baseType: Type = ScalarType.bool

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(value = f(value)))

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

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(arguments = arguments.map(_.map(f))))

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

case class CallServiceRaw(
  serviceId: ValueRaw,
  fnName: String,
  baseType: ArrowType,
  arguments: List[ValueRaw]
) extends ValueRaw {
  override def `type`: Type = baseType.codomain.headOption.getOrElse(baseType)

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(
      copy(
        serviceId = serviceId.map(f),
        arguments = arguments.map(_.map(f))
      )
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
