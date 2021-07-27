package aqua.model

import aqua.types._
import cats.Eq
import cats.data.{Chain, NonEmptyMap}
import wvlet.log.LogSupport

sealed trait ValueModel {
  def `type`: Type

  def lastType: Type

  def resolveWith(map: Map[String, ValueModel]): ValueModel = this
}

object ValueModel {

  implicit object ValueModelEq extends Eq[ValueModel] {
    override def eqv(x: ValueModel, y: ValueModel): Boolean = x == y
  }

  def varName(vm: ValueModel): Option[String] =
    vm match {
      case VarModel(name, _, _) => Some(name)
      case _ => None
    }
}

case class LiteralModel(value: String, `type`: Type) extends ValueModel {
  override def lastType: Type = `type`

  override def toString: String = s"{$value: ${`type`}}"
}

object LiteralModel {
  def quote(str: String): LiteralModel = LiteralModel("\"" + str + "\"", ScalarType.string)

  val initPeerId: LiteralModel = LiteralModel("%init_peer_id%", ScalarType.string)
}

sealed trait LambdaModel {
  def `type`: Type
}
case class IntoArrayModel(`type`: Type) extends LambdaModel
case class IntoFieldModel(field: String, `type`: Type) extends LambdaModel
case class IntoIndexModel(idx: Int, `type`: Type) extends LambdaModel

case class VarModel(name: String, `type`: Type, lambda: Chain[LambdaModel] = Chain.empty)
    extends ValueModel with LogSupport {
  def deriveFrom(vm: VarModel): VarModel = vm.copy(lambda = vm.lambda ++ lambda)

  override val lastType: Type = lambda.lastOption.map(_.`type`).getOrElse(`type`)

  override def resolveWith(map: Map[String, ValueModel]): ValueModel =
    map.get(name) match {
      case Some(vv: VarModel) =>
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
              case vm @ VarModel(nn, _, _) if nn == name => deriveFrom(vm)
              // it couldn't go to a cycle as long as the semantics protects it
              case _ =>
                n.resolveWith(map) match {
                  case nvm: VarModel =>
                    deriveFrom(nvm)
                  case valueModel =>
                    if (lambda.nonEmpty)
                      error(
                        s"Var $name derived from scalar $valueModel, but lambda is lost: $lambda"
                      )
                    valueModel
                }
            }
          case _ =>
            deriveFrom(vv)
        }

      case Some(vv) => vv // TODO check that lambda is empty, otherwise error
      case None => this // Should not happen
    }
}

object VarModel {

  val lastError: VarModel = VarModel(
    "%last_error%",
    ProductType(
      "LastError",
      NonEmptyMap.of(
        "instruction" -> ScalarType.string,
        "msg" -> ScalarType.string,
        "peer_id" -> ScalarType.string
      )
    )
  )

  val nil: VarModel = VarModel(
    "nil",
    StreamType(DataType.Bottom)
  )
}
