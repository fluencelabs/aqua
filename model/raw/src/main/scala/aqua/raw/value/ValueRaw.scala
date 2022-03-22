package aqua.raw.value

import aqua.types.*
import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import cats.Eq
import scribe.Logging

sealed trait ValueRaw {

  def baseType: Type

  def `type`: Type = baseType

  def renameVars(map: Map[String, String]): ValueRaw = this

  def map(f: ValueRaw => ValueRaw): ValueRaw

  def varNames: Set[String]

}

object ValueRaw {

  // TODO: move to LiteralRaw
  val InitPeerId: LiteralRaw = LiteralRaw("%init_peer_id%", ScalarType.string)

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

case class ApplyLambdaRaw(value: ValueRaw, lambda: LambdaRaw) extends ValueRaw {
  override def baseType: Type = value.baseType

  override def `type`: Type = lambda.`type`

  override def renameVars(map: Map[String, String]): ValueRaw =
    ApplyLambdaRaw(value.renameVars(map), lambda.renameVars(map))

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(ApplyLambdaRaw(f(value), lambda.map(f)))

  override def toString: String = s"$value.$lambda"

  def unwind: (ValueRaw, Chain[LambdaRaw]) = value match {
    case alr: ApplyLambdaRaw =>
      val (v, i) = alr.unwind
      (v, i :+ lambda)
    case _ =>
      (value, Chain.one(lambda))
  }

  override def varNames: Set[String] = value.varNames ++ lambda.varNames
}

object ApplyLambdaRaw {

  def fromChain(value: ValueRaw, lambdas: Chain[LambdaRaw]): ValueRaw =
    lambdas.foldLeft(value) { case (v, l) =>
      ApplyLambdaRaw(v, l)
    }
}

case class VarRaw(name: String, baseType: Type) extends ValueRaw {

  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(this)

  override def renameVars(map: Map[String, String]): ValueRaw =
    copy(map.getOrElse(name, name))

  override def toString: String = s"var{$name: " + baseType + s"}"

  def withLambda(lambda: LambdaRaw*): ValueRaw =
    ApplyLambdaRaw.fromChain(this, Chain.fromSeq(lambda))

  override def varNames: Set[String] = Set(name)
}

case class LiteralRaw(value: String, baseType: Type) extends ValueRaw {
  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(this)

  override def toString: String = s"{$value: ${baseType}}"

  override def varNames: Set[String] = Set.empty
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
}
