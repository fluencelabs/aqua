package aqua.model

import aqua.raw.value.*
import aqua.types.*
import cats.Eq
import cats.data.{Chain, NonEmptyMap}
import scribe.Logging

sealed trait ValueModel {
  def `type`: Type

  def lastType: Type
}

object ValueModel {

  implicit object ValueModelEq extends Eq[ValueModel] {
    override def eqv(x: ValueModel, y: ValueModel): Boolean = x == y
  }

  def fromRaw(raw: ValueRaw): ValueModel = raw match {
    case VarRaw(name, t, lambda) =>
      // TODO: handle recursive lambda
      VarModel(name, t, lambda.map(LambdaModel.fromRaw))
    case LiteralRaw(value, t) =>
      LiteralModel(value, t)
  }
}

case class LiteralModel(value: String, `type`: Type) extends ValueModel {
  override def lastType: Type = `type`

  override def toString: String = s"{$value: ${`type`}}"
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

case class VarModel(name: String, `type`: Type, lambda: Chain[LambdaModel]) extends ValueModel {

  override val lastType: Type = lambda.lastOption.map(_.`type`).getOrElse(`type`)

  override def toString: String = s"var{$name: " + `type` + s"}.${lambda.toList.mkString(".")}"
}
