package aqua.raw.value

import aqua.types.*
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import cats.Eq
import scribe.Logging

sealed trait ValueRaw {

  def baseType: Type

  def `type`: Type = baseType

  def renameVars(map: Map[String, String]): ValueRaw

  def map(f: ValueRaw => ValueRaw): ValueRaw

  def varNames: Set[String]
}

object ValueRaw {

  // TODO: move to LiteralRaw
  val InitPeerId: LiteralRaw = LiteralRaw("%init_peer_id%", ScalarType.string)
  val ParticleTtl: LiteralRaw = LiteralRaw("%ttl%", ScalarType.u32)
  val ParticleTimestamp: LiteralRaw = LiteralRaw("%timestamp%", ScalarType.u64)

  val Nil: LiteralRaw = LiteralRaw("[]", StreamType(BottomType))

  val LastError: VarRaw = VarRaw(
    "%last_error%",
    StructType(
      "LastError",
      NonEmptyMap.of(
        // These two fields are mandatory for all errors
        "message" -> ScalarType.string,
        "error_code" -> ScalarType.i64,
        // These fields are specific to AquaVM's errors only
        "instruction" -> ScalarType.string,
        "peer_id" -> ScalarType.string
      )
    )
  )

}

case class ApplyPropertyRaw(value: ValueRaw, property: PropertyRaw) extends ValueRaw {
  override def baseType: Type = value.baseType

  override def `type`: Type = property.`type`

  override def renameVars(map: Map[String, String]): ValueRaw =
    ApplyPropertyRaw(value.renameVars(map), property.renameVars(map))

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(
    ApplyPropertyRaw(f(value), property.map(f))
  )

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

  override def map(f: ValueRaw => ValueRaw): ValueRaw = this

  override def toString: String = s"gate $name.$idx"

  override def varNames: Set[String] = Set(name) ++ idx.varNames
}

case class VarRaw(name: String, baseType: Type) extends ValueRaw {

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(this)

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(map.getOrElse(name, name))

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

  def number(value: Int): LiteralRaw = LiteralRaw(value.toString, LiteralType.number)

  val Zero: LiteralRaw = LiteralRaw("0", LiteralType.number)

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

case class AbilityRaw(fieldsAndArrows: NonEmptyMap[String, ValueRaw], abilityType: AbilityType) extends ValueRaw {

  override def baseType: Type = abilityType

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(copy(fieldsAndArrows = fieldsAndArrows.map(f)))

  override def varNames: Set[String] = {
    fieldsAndArrows.toSortedMap.values.flatMap(_.varNames).toSet
  }

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(fieldsAndArrows = fieldsAndArrows.map(_.renameVars(map)))
}

case class CallArrowRaw(
  // TODO: ability should hold a type, not name
  ability: Option[String],
  name: String,
  arguments: List[ValueRaw],
  baseType: ArrowType,
  // TODO: there should be no serviceId there
  serviceId: Option[ValueRaw]
) extends ValueRaw {
  override def `type`: Type = baseType.codomain.uncons.map(_._1).getOrElse(baseType)

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(arguments = arguments.map(f)))

  override def varNames: Set[String] = arguments.flatMap(_.varNames).toSet

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(
      name = map
        .get(name)
        // Rename only if it is **not** a service or ability call, see [bug LNG-199]
        .filterNot(_ => ability.isDefined)
        .getOrElse(name),
      arguments = arguments.map(_.renameVars(map)),
      serviceId = serviceId.map(_.renameVars(map))
    )

  override def toString: String =
    s"(call ${ability.fold("")(a => s"|$a| ")} (${serviceId.fold("")(_.toString + " ")}$name) [${arguments
      .mkString(" ")}] :: $baseType)"
}
