package aqua.raw.value

import aqua.types.{BottomType, LiteralType, ScalarType, StreamType, StructType, Type}
import cats.data.{Chain, NonEmptyMap}
import cats.Eq
import scribe.Logging

sealed trait ValueRaw {

  def usesVarNames: Set[String] = Set.empty

  def `type`: Type

  def lastType: Type = `type`

  def renameVars(map: Map[String, String]): ValueRaw = this

  def map(f: ValueRaw => ValueRaw): ValueRaw

}

object ValueRaw {

  implicit object ValueRawEq extends Eq[ValueRaw] {
    override def eqv(x: ValueRaw, y: ValueRaw): Boolean = x == y
  }

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

case class VarRaw(name: String, `type`: Type, lambda: Chain[LambdaRaw] = Chain.empty)
    extends ValueRaw with Logging {

  override def usesVarNames: Set[String] =
    lambda.toList.map(_.usesVarNames).foldLeft(Set(name))(_ ++ _)

  override val lastType: Type = lambda.lastOption.map(_.`type`).getOrElse(`type`)

  override def map(f: ValueRaw => ValueRaw): ValueRaw =
    f(copy(lambda = lambda.map(_.map(f))))

  override def renameVars(map: Map[String, String]): ValueRaw =
    VarRaw(map.getOrElse(name, name), `type`, lambda.map(_.renameVars(map)))

  override def toString: String = s"var{$name: " + `type` + s"}.${lambda.toList.mkString(".")}"
}

case class LiteralRaw(value: String, `type`: Type) extends ValueRaw {
  override def map(f: ValueRaw => ValueRaw): ValueRaw = f(this)

  override def toString: String = s"{$value: ${`type`}}"
}

object LiteralRaw {
  def quote(value: String): LiteralRaw = LiteralRaw("\"" + value + "\"", LiteralType.string)

  def number(value: Int): LiteralRaw = LiteralRaw(value.toString, LiteralType.number)
  val Zero: LiteralRaw = LiteralRaw("0", LiteralType.number)

  val True: LiteralRaw = LiteralRaw("true", LiteralType.bool)
  val False: LiteralRaw = LiteralRaw("false", LiteralType.bool)
}
