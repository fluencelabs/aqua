package aqua.raw.value

import aqua.types.{BottomType, LiteralType, ScalarType, StreamType, StructType, Type}
import cats.data.{Chain, NonEmptyMap}
import cats.Eq
import scribe.Logging

sealed trait ValueRaw {

  def usesVarNames: Set[String] = Set.empty

  def resolveWith(map: Map[String, ValueRaw]): ValueRaw = this

  def `type`: Type

  def lastType: Type = `type`

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
  override def usesVarNames: Set[String] = Set(name)

  override val lastType: Type = lambda.lastOption.map(_.`type`).getOrElse(`type`)

  def deriveFrom(vm: VarRaw): VarRaw = vm.copy(lambda = vm.lambda ++ lambda)

  override def resolveWith(map: Map[String, ValueRaw]): ValueRaw =
    map.get(name) match {
      case Some(vv: VarRaw) =>
        map.get(vv.name) match {
          case Some(n) =>
            n match {
              /* This case protects from infinite recursion
                 when similar names are in a body of a function and a call of a function
                service Demo("demo"):
                  get4: u64 -> u64

                func two(variable: u64) -> u64:
                    v <- Demo.get4(variable)
                    <- variable

                func three(v: u64) -> u64:
                    variable <- Demo.get4(v)
                    -- here we will try to resolve 'variable' to VarModel('variable')
                    -- that could cause infinite recursion
                    res <- two(variable)
                    <- variable
               */
              case vm @ VarRaw(nn, _, _) if nn == name => deriveFrom(vm)
              // it couldn't go to a cycle as long as the semantics protects it
              case _ =>
                n.resolveWith(map) match {
                  case nvm: VarRaw =>
                    deriveFrom(nvm)
                  case valueModel =>
                    if (lambda.nonEmpty)
                      logger.error(
                        s"Var $name derived from scalar $valueModel, but lambda is lost: $lambda"
                      )
                    valueModel
                }
            }
          case _ =>
            deriveFrom(vv)
        }

      case Some(vv) =>
        vv // TODO check that lambda is empty, otherwise error
      case None =>
        this // Should not happen
    }

  override def toString(): String = s"var{$name: " + `type` + s"}.${lambda.toList.mkString(".")}"
}

case class LiteralRaw(value: String, `type`: Type) extends ValueRaw {
  override def toString: String = s"{$value: ${`type`}}"
}

object LiteralRaw {
  def quote(value: String): LiteralRaw = LiteralRaw("\"" + value + "\"", LiteralType.string)
  def number(value: Int): LiteralRaw = LiteralRaw(value.toString, LiteralType.number)
  val True: LiteralRaw = LiteralRaw("true", LiteralType.bool)
  val False: LiteralRaw = LiteralRaw("false", LiteralType.bool)
}
