package aqua.backend

import aqua.res.FuncRes
import aqua.types.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

import scala.annotation.tailrec

// Represents the Aqua types
sealed trait TypeDefinition {
  def tag: String
}

object TypeDefinition {

  implicit val encodeProdDefType: Encoder[ProductTypeDef] = {
    case d @ LabelledProductTypeDef(fields) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("fields", Json.fromFields(fields.map { case (n, t) => (n, t.asJson) }))
      )
    case d @ UnlabelledProductTypeDef(items) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("items", Json.fromValues(items.map(_.asJson)))
      )
    case d @ NilTypeDef =>
      Json.obj(
        ("tag", Json.fromString(d.tag))
      )
  }

  implicit val encodeDefType: Encoder[TypeDefinition] = {
    case d @ ScalarTypeDef(name) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("name", Json.fromString(name))
      )
    case d @ ArrayTypeDef(t) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("type", t.asJson)
      )
    case d @ StructTypeDef(name, fields) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("name", Json.fromString(name)),
        ("fields", Json.fromFields(fields.toList.map { case (n, t) => (n, t.asJson) }))
      )

    case d @ OptionalType(t) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("type", t.asJson)
      )
    case d @ ArrowTypeDef(domain, codomain) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("domain", domain.asJson),
        ("codomain", codomain.asJson)
      )
    case d =>
      Json.obj(
        ("tag", Json.fromString(d.tag))
      )
  }

  implicit val encodeServiceDefType: Encoder[ServiceDef] = { case ServiceDef(sId, functions) =>
    Json.obj(
      ("defaultServiceId", sId.asJson),
      ("functions", encodeProdDefType(functions))
    )
  }

  implicit val encodeNamesConfig: Encoder[NamesConfig] = { case n: NamesConfig =>
    import n.*
    Json.obj(
      ("relay", Json.fromString(relay)),
      ("getDataSrv", Json.fromString(getDataSrv)),
      ("callbackSrv", Json.fromString(callbackSrv)),
      ("responseSrv", Json.fromString(responseSrv)),
      ("responseFnName", Json.fromString(responseFnName)),
      ("errorHandlingSrv", Json.fromString(errorHandlingSrv)),
      ("errorFnName", Json.fromString(errorFnName))
    )
  }

  implicit val encodeFunctionDefType: Encoder[FunctionDef] = {
    case FunctionDef(fName, arrow, names) =>
      Json.obj(
        ("functionName", Json.fromString(fName)),
        ("arrow", encodeDefType(arrow)),
        ("names", names.asJson)
      )
  }

  def apply(t: Option[Type]): TypeDefinition = t.map(apply).getOrElse(NilTypeDef)

  def apply(t: Type): TypeDefinition = {
    t match {
      case OptionType(t) =>
        OptionalType(TypeDefinition(t))
      case t: BoxType => ArrayTypeDef(TypeDefinition(t.element))
      case StructType(name, fields) =>
        StructTypeDef(name, fields.toSortedMap.view.mapValues(TypeDefinition.apply).toMap)
      case t: ScalarType => ScalarTypeDef.fromScalar(t)
      case t: ProductType => ProductTypeDef(t)
      case t: ArrowType =>
        ArrowTypeDef(t)
      case TopType => TopTypeDef
      case BottomType => BottomTypeDef
    }
  }
}

sealed trait ProductTypeDef extends TypeDefinition

object ProductTypeDef {

  def apply(t: ProductType): ProductTypeDef = {
    t match {
      case lt: LabelledConsType =>
        LabelledProductTypeDef(
          lt.toLabelledList().map { case (n, t) => (n, TypeDefinition(t)) }
        )
      case ut: UnlabelledConsType =>
        UnlabelledProductTypeDef(ut.toList.map(TypeDefinition.apply))
      case NilType => NilTypeDef
    }
  }
}

case class OptionalType(t: TypeDefinition) extends TypeDefinition { val tag = "option" }
case class ScalarTypeDef private (name: String) extends TypeDefinition { val tag = "scalar" }

object ScalarTypeDef {
  def fromScalar(s: ScalarType) = ScalarTypeDef(s.name)
}

case class ArrayTypeDef(`type`: TypeDefinition) extends TypeDefinition { val tag = "array" }

case class ArrowTypeDef(
  domain: ProductTypeDef,
  codomain: ProductTypeDef
) extends TypeDefinition {
  val tag = "arrow"
}

object ArrowTypeDef {

  def apply(at: ArrowType): ArrowTypeDef =
    ArrowTypeDef(ProductTypeDef(at.domain), ProductTypeDef(at.codomain))
}

case object NilTypeDef extends ProductTypeDef {
  val tag = "nil"
}

case object TopTypeDef extends TypeDefinition {
  val tag = "top"
}

case object BottomTypeDef extends TypeDefinition {
  val tag = "bottom"
}

case class StructTypeDef(name: String, fields: Map[String, TypeDefinition]) extends TypeDefinition {
  val tag = "struct"
}

case class LabelledProductTypeDef(fields: List[(String, TypeDefinition)]) extends ProductTypeDef {
  val tag = "labeledProduct"
}

case class UnlabelledProductTypeDef(items: List[TypeDefinition]) extends ProductTypeDef {
  val tag = "unlabeledProduct"
}

// Names of services and functions that are used in air
case class NamesConfig(
  relay: String,
  getDataSrv: String,
  callbackSrv: String,
  responseSrv: String,
  responseFnName: String,
  errorHandlingSrv: String,
  errorFnName: String
)

// Describes service
case class ServiceDef(defaultServiceId: Option[String], functions: LabelledProductTypeDef)

// Describes top-level function
case class FunctionDef(
  functionName: String,
  arrow: ArrowTypeDef,
  names: NamesConfig
)

object FunctionDef {

  def apply(func: FuncRes): FunctionDef = {
    val args = LabelledProductTypeDef(func.args.map(a => (a.name, TypeDefinition(a.`type`))))

    val names = NamesConfig(
      func.relayVarName.getOrElse("-relay-"),
      func.dataServiceId,
      func.callbackServiceId,
      func.callbackServiceId,
      func.respFuncName,
      func.errorHandlerId,
      func.errorFuncName
    )
    FunctionDef(
      func.funcName,
      ArrowTypeDef(args, ProductTypeDef(func.returnType)),
      names
    )
  }
}
