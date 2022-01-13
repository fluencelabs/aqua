package aqua.model

import aqua.raw.value.*
import aqua.types.*
import cats.Eq
import cats.data.{Chain, NonEmptyMap}
import scribe.Logging

sealed trait ValueModel {
  def `type`: Type

  def lastType: Type

  def resolveWith(map: Map[String, ValueModel]): ValueModel = this
}

object ValueModel {

  implicit object ValueModelEq extends Eq[ValueModel] {
    override def eqv(x: ValueModel, y: ValueModel): Boolean = x == y
  }

  // TODO it should be marked with DANGEROUS signs and so on, as THIS IS UNSAFE!!!!!!!!!!!!!!! usable only for tests
  def fromRaw(raw: ValueRaw): ValueModel = raw match {
    case VarRaw(name, t, lambda) =>
      VarModel(name, t, lambda.map(LambdaModel.fromRaw))
    case LiteralRaw(value, t) =>
      LiteralModel(value, t)
  }

}

case class LiteralModel(value: String, `type`: Type) extends ValueModel {
  override def lastType: Type = `type`

  override def toString: String = s"{$value: ${`type`}}"
}

object LiteralModel {
  def fromRaw(raw: LiteralRaw): LiteralModel = LiteralModel(raw.value, raw.`type`)
}

sealed trait LambdaModel {
  def `type`: Type
}

object LambdaModel {

  def fromRaw(l: LambdaRaw): LambdaModel = l match {
    case IntoFieldRaw(field, t) => IntoFieldModel(field, t)
    case IntoIndexRaw(idx, t) =>
      // TODO: handle recursive lambda
      IntoIndexModel(
        ValueModel.fromRaw(idx) match {
          case VarModel(name, _, _) => name
          case LiteralModel(value, _) => value
        },
        t
      )
  }

}

case class IntoFieldModel(field: String, `type`: Type) extends LambdaModel

case class IntoIndexModel(idx: String, `type`: Type) extends LambdaModel

case class VarModel(name: String, `type`: Type, lambda: Chain[LambdaModel])
    extends ValueModel with Logging {

  override val lastType: Type = lambda.lastOption.map(_.`type`).getOrElse(`type`)

  override def toString: String = s"var{$name: " + `type` + s"}.${lambda.toList.mkString(".")}"

  private def deriveFrom(vm: VarModel): VarModel =
    vm.copy(lambda = vm.lambda ++ lambda)

  override def resolveWith(vals: Map[String, ValueModel]): ValueModel =
    vals.get(name) match {
      case Some(vv: VarModel) =>
        vals.get(vv.name) match {
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
                n.resolveWith(vals) match {
                  case nvm: VarModel =>
                    deriveFrom(nvm)
                  case valueModel =>
                    if (lambda.nonEmpty)
                      logger.error(
                        s"Var $name derived from literal $valueModel, but lambda is lost: $lambda"
                      )
                    valueModel
                }
            }
          case _ =>
            deriveFrom(vv)
        }

      case Some(vv) =>
        if (lambda.nonEmpty)
          logger.error(
            s"Var $name derived from literal $vv, but lambda is lost: $lambda"
          )
        vv
      case None =>
        this // Should not happen
    }
}
