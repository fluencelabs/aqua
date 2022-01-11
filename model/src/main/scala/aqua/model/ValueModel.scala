package aqua.model

import aqua.raw.value.*
import aqua.types.*
import cats.Eq
import cats.data.{Chain, NonEmptyMap}
import scribe.Logging

sealed trait ValueModel {
  def `type`: Type

  def lastType: Type

  // This is a debt: it should never be used
  def toRaw: ValueRaw
}

object ValueModel {

  implicit object ValueModelEq extends Eq[ValueModel] {
    override def eqv(x: ValueModel, y: ValueModel): Boolean = x == y
  }

  def recursiveRaw(raw: ValueRaw, i: Int): (ValueModel, Map[String, ValueRaw]) = raw match {
    case VarRaw(name, t, lambda) if lambda.isEmpty =>
      VarModel(name, t, Chain.empty) -> Map.empty
    case LiteralRaw(value, t) =>
      LiteralModel(value, t) -> Map.empty
    case VarRaw(name, t, lambda) =>
      val (lambdaModel, map) =
        lambda.foldLeft(Chain.empty[LambdaModel] -> Map.empty[String, ValueRaw]) {
          case ((lc, m), l) =>
            val (lm, mm) = LambdaModel.flattenRaw(l, i + m.size)
            (lc :+ lm, m ++ mm)
        }
      VarModel(name, t, lambdaModel) -> map
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

  override def toRaw: ValueRaw = LiteralRaw(value, `type`)

  override def toString: String = s"{$value: ${`type`}}"
}

sealed trait LambdaModel {
  def `type`: Type

  def toRaw: LambdaRaw
}

object LambdaModel {

  def flattenRaw(l: LambdaRaw, i: Int): (LambdaModel, Map[String, ValueRaw]) = l match {
    case IntoFieldRaw(field, t) => IntoFieldModel(field, t) -> Map.empty
    case IntoIndexRaw(vm @ VarRaw(name, _, l), t) if l.nonEmpty =>
      val ni = name + "-" + i
      IntoIndexModel(ni, t) -> Map(ni -> vm)
    case IntoIndexRaw(VarRaw(name, _, _), t) =>
      IntoIndexModel(name, t) -> Map.empty

    case IntoIndexRaw(LiteralRaw(value, _), t) =>
      IntoIndexModel(value, t) -> Map.empty
  }

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
  override def toRaw: LambdaRaw = IntoFieldRaw(field, `type`)
}

case class IntoIndexModel(idx: String, `type`: Type) extends LambdaModel {

  override def toRaw: LambdaRaw = IntoIndexRaw(
    if (idx.forall(Character.isDigit)) LiteralRaw.number(idx.toInt)
    else VarRaw(idx, ScalarType.string),
    `type`
  )
}

case class VarModel(name: String, `type`: Type, lambda: Chain[LambdaModel]) extends ValueModel {

  override val lastType: Type = lambda.lastOption.map(_.`type`).getOrElse(`type`)

  override def toRaw: ValueRaw = VarRaw(name, `type`, lambda.map(_.toRaw))

  override def toString: String = s"var{$name: " + `type` + s"}.${lambda.toList.mkString(".")}"
}
