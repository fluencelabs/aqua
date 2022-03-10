package aqua.model

import aqua.raw.value.*
import aqua.types.*
import cats.Eq
import cats.data.{Chain, NonEmptyMap}
import scribe.Logging

sealed trait ValueModel {
  def `type`: Type

  def resolveWith(map: Map[String, ValueModel]): ValueModel = this

  def usesVarNames: Set[String] = Set.empty
}

object ValueModel {

  implicit object ValueModelEq extends Eq[ValueModel] {
    override def eqv(x: ValueModel, y: ValueModel): Boolean = x == y
  }

  // TODO it should be marked with DANGEROUS signs and so on, as THIS IS UNSAFE!!!!!!!!!!!!!!! usable only for tests
  def fromRaw(raw: ValueRaw): ValueModel = raw match {
    case ApplyLambdaRaw(v, lambda) =>
      fromRaw(v) match {
        case vm: VarModel => vm.copy(lambda = vm.lambda :+ LambdaModel.fromRaw(lambda))
        case _ => ???
      }
    case VarRaw(name, t) =>
      VarModel(name, t)
    case LiteralRaw(value, t) =>
      LiteralModel(value, t)
    case _ => ???
  }

}

case class LiteralModel(value: String, `type`: Type) extends ValueModel {

  override def toString: String = s"{$value: ${`type`}}"
}

object LiteralModel {
  def fromRaw(raw: LiteralRaw): LiteralModel = LiteralModel(raw.value, raw.baseType)
}

sealed trait LambdaModel {
  def usesVarNames: Set[String] = Set.empty

  def `type`: Type

  def toRaw: LambdaRaw
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

case class IntoFieldModel(field: String, `type`: Type) extends LambdaModel {
  override def toString: String = s".$field:${`type`}"

  override def toRaw: LambdaRaw = IntoFieldRaw(field, `type`)
}

case class IntoIndexModel(idx: String, `type`: Type) extends LambdaModel {
  override lazy val usesVarNames: Set[String] = Set(idx).filterNot(_.forall(Character.isDigit))

  override def toString: String = s"[$idx -> ${`type`}]"

  override def toRaw: LambdaRaw = IntoIndexRaw(
    if (idx.forall(Character.isDigit)) LiteralRaw(idx, LiteralType.number)
    else VarRaw(idx, LiteralType.number),
    `type`
  )
}

case class VarModel(name: String, baseType: Type, lambda: Chain[LambdaModel] = Chain.empty)
  extends ValueModel with Logging {

  override lazy val usesVarNames: Set[String] =
    lambda.toList.map(_.usesVarNames).foldLeft(Set(name))(_ ++ _)

  override val `type`: Type = lambda.lastOption.map(_.`type`).getOrElse(baseType)

  override def toString: String = s"var{$name: " + baseType + s"}.${lambda.toList.mkString(".")}"

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
              case vm@VarModel(nn, _, _) if nn == name => deriveFrom(vm)
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
