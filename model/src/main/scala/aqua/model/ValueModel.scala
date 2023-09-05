package aqua.model

import aqua.raw.value.*
import aqua.types.*

import cats.Eq
import cats.data.{Chain, NonEmptyMap}
import cats.syntax.option.*
import scribe.Logging

sealed trait ValueModel {
  def `type`: Type

  def resolveWith(map: Map[String, ValueModel]): ValueModel = this

  def usesVarNames: Set[String] = Set.empty

  def toRaw: ValueRaw
}

object ValueModel {

  def errorCode(error: VarModel): Option[VarModel] =
    error.intoField("error_code")

  val lastError = VarModel(
    name = ValueRaw.lastError.name,
    baseType = ValueRaw.lastError.baseType
  )

  val lastErrorType = ValueRaw.lastErrorType

  // NOTE: It should be safe as %last_error% should have `error_code` field
  val lastErrorCode = errorCode(lastError).get

  implicit object ValueModelEq extends Eq[ValueModel] {
    override def eqv(x: ValueModel, y: ValueModel): Boolean = x == y
  }

  // TODO it should be marked with DANGEROUS signs and so on, as THIS IS UNSAFE!!!!!!!!!!!!!!! usable only for tests
  def fromRaw(raw: ValueRaw): ValueModel = raw match {
    case ApplyPropertyRaw(v, property) =>
      fromRaw(v) match {
        case vm: VarModel => vm.copy(properties = vm.properties :+ PropertyModel.fromRaw(property))
        case _ => ???
      }
    case VarRaw(name, t) =>
      VarModel(name, t)
    case LiteralRaw(value, t) =>
      LiteralModel(value, t)
    case p =>
      println("catch: " + p)
      ???
  }

  object Ability {

    def unapply(vm: VarModel): Option[(String, AbilityType, Chain[PropertyModel])] =
      vm match {
        case VarModel(name, t@AbilityType(_, _), properties) =>
          (name, t, properties).some
        case _ => none
      }
  }
}

case class LiteralModel(value: String, `type`: Type) extends ValueModel {

  override def toString: String = s"{$value: ${`type`}}"

  def toRaw: ValueRaw = LiteralRaw(value, `type`)
}

object LiteralModel {

  /**
   * Used to match bool literals in pattern matching
   */
  object Bool {

    def unapply(lm: LiteralModel): Option[Boolean] =
      lm match {
        case LiteralModel("true", ScalarType.bool | LiteralType.bool) => true.some
        case LiteralModel("false", ScalarType.bool | LiteralType.bool) => false.some
        case _ => none
      }
  }

  // AquaVM will return empty string for
  // %last_error%.$.error_code if there is no %last_error%
  val emptyErrorCode = quote("")

  def fromRaw(raw: LiteralRaw): LiteralModel = LiteralModel(raw.value, raw.baseType)

  def quote(str: String): LiteralModel = LiteralModel(s"\"$str\"", LiteralType.string)

  def number(n: Int): LiteralModel = LiteralModel(n.toString, LiteralType.forInt(n))

  def bool(b: Boolean): LiteralModel = LiteralModel(b.toString.toLowerCase, LiteralType.bool)
}

sealed trait PropertyModel {
  def usesVarNames: Set[String] = Set.empty

  def `type`: Type

  def toRaw: PropertyRaw
}

object PropertyModel {

  def fromRaw(l: PropertyRaw): PropertyModel = l match {
    case FunctorRaw(op, t) => FunctorModel(op, t)
    case IntoFieldRaw(field, t) => IntoFieldModel(field, t)
    case IntoIndexRaw(idx, t) =>
      // TODO: handle recursive property
      IntoIndexModel(
        ValueModel.fromRaw(idx) match {
          case VarModel(name, _, _) => name
          case LiteralModel(value, _) => value
        },
        t
      )
  }

}

case class FunctorModel(name: String, `type`: Type) extends PropertyModel {
  override def toString: String = s".$name:${`type`}"

  override def toRaw: PropertyRaw = FunctorRaw(name, `type`)
}

case class IntoFieldModel(name: String, `type`: Type) extends PropertyModel {
  override def toString: String = s".$name:${`type`}"

  override def toRaw: PropertyRaw = IntoFieldRaw(name, `type`)
}

case class IntoIndexModel(idx: String, `type`: Type) extends PropertyModel {
  override lazy val usesVarNames: Set[String] = Set(idx).filterNot(_.forall(Character.isDigit))

  override def toString: String = s"[$idx -> ${`type`}]"

  override def toRaw: PropertyRaw = IntoIndexRaw(
    if (idx.forall(Character.isDigit)) LiteralRaw(idx, LiteralType.number)
    else VarRaw(idx, LiteralType.number),
    `type`
  )
}

case class VarModel(name: String, baseType: Type, properties: Chain[PropertyModel] = Chain.empty)
    extends ValueModel with Logging {

  override lazy val usesVarNames: Set[String] =
    properties.toList.map(_.usesVarNames).foldLeft(Set(name))(_ ++ _)

  override val `type`: Type = properties.lastOption.map(_.`type`).getOrElse(baseType)

  def toRaw: ValueRaw = VarRaw(name, baseType).withProperty(properties.map(_.toRaw).toList: _*)

  override def toString: String =
    s"var{$name: " + baseType + s"}.${properties.toList.mkString(".")}"

  private def deriveFrom(vm: VarModel): VarModel =
    vm.copy(properties = vm.properties ++ properties)

  def intoField(field: String): Option[VarModel] = `type` match {
    case StructType(_, fields) =>
      fields(field)
        .map(fieldType =>
          copy(
            properties = properties :+ IntoFieldModel(field, fieldType)
          )
        )
    case _ => none
  }

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
              case vm @ VarModel(nn, _, _) if nn == name => deriveFrom(vv.deriveFrom(vm))
              // it couldn't go to a cycle as long as the semantics protects it
              case _ =>
                n.resolveWith(vals) match {
                  case nvm: VarModel =>
                    deriveFrom(vv.deriveFrom(nvm))
                  case valueModel =>
                    if (properties.nonEmpty)
                      logger.error(
                        s"Var $name derived from literal $valueModel, but property is lost: $properties"
                      )
                    valueModel
                }
            }
          case _ =>
            deriveFrom(vv)
        }

      case Some(vv) =>
        if (properties.nonEmpty)
          logger.error(
            s"Var $name derived from literal $vv, but property is lost: $properties"
          )
        vv
      case None =>
        this // Should not happen
    }
}
