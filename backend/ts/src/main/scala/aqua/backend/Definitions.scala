package aqua.backend

import aqua.res.FuncRes
import aqua.types.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

import scala.annotation.tailrec

// Represents the Aqua types
sealed trait TypeDefinition {
  def tag: String
}

object TypeDefinition {

  implicit val encodeDefType: Encoder[TypeDefinition] = {
    case d @ (NilTypeDef, TopTypeDef, BottomTypeDef) =>
      Json.obj(
        ("tag", Json.fromString(d.tag))
      )
    case d @ CallbackType(cDef) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("callback", cDef.asJson)
      )
    case d @ MultiReturnType(returnItems) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("returnItems", returnItems.asJson)
      )
  }

  def apply(t: Option[Type]): TypeDefinition = t.map(apply).getOrElse(NilType)

  def apply(t: Type): TypeDefinition = {
    t match {
      case OptionType(t) =>
        OptionalType(TypeDefinition(t))
      case t: BoxType => ArrayTypeDef(TypeDefinition(t.element))
      case StructType(name, fields) =>
        StructTypeDef(name, fields.toMap.view.mapValues(TypeDefinition.apply))
      case t: ScalarType => ScalarTypeDef.fromScalar(t)
      case t: ProductType => ProductTypeDef(t)
      case ArrowType(d, cd) =>
        ArrowTypeDef(ProductTypeDef(d), ProductTypeDef(cd))
      case t: TopType => TopTypeDef
      case t: BottomType => BottomTypeDef
    }
  }
}

type ProductTypeDef = LabelledProductTypeDef | UnlabelledProductTypeDef | NilTypeDef

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

case class ArrayTypeDef(t: TypeDefinition) extends TypeDefinition { val tag = "array" }

case class ArrowTypeDef(domain: ProductTypeDef, codomain: ProductTypeDef) extends TypeDefinition {
  val tag = "arrow"
}

case object NilTypeDef extends TypeDefinition {
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

case class LabelledProductTypeDef(items: List[(String, TypeDefinition)]) extends TypeDefinition {
  val tag = "labeledProduct"
}

case class UnlabelledProductTypeDef(items: List[TypeDefinition]) extends TypeDefinition {
  val tag = "unlabeledProduct"
}

// Describes callbacks that passes as arguments in functions
case class CallbackDefinition(argDefs: List[ArgDefinition], returnType: TypeDefinition)

object CallbackDefinition {

  implicit val encodeCallbackDef: Encoder[CallbackDefinition] = (cbDef: CallbackDefinition) =>
    Json.obj(
      ("argDefs", cbDef.argDefs.asJson),
      ("returnType", cbDef.returnType.asJson)
    )

  def apply(arrow: ArrowType): CallbackDefinition = {
    val args = arrow.domain.toLabelledList().map(arg => ArgDefinition.argToDef(arg._1, arg._2))
    val returns = arrow.codomain.toList
    val returnType = returns match {
      case head :: Nil =>
        TypeDefinition(head)
      case Nil =>
        NilTypeDef
      case _ =>
        UnlabelledProductTypeDef(returns.map(TypeDefinition.apply))
    }
    CallbackDefinition(args, returnType)
  }
}

// Describes arguments in functions and callbacks
case class ArgDefinition(
  name: String,
  argType: TypeDefinition
)

object ArgDefinition {

  implicit val encodeArgDef: Encoder[ArgDefinition] = (a: ArgDefinition) =>
    Json.obj(
      ("name", Json.fromString(a.name)),
      ("argType", a.argType.asJson)
    )
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
case class ServiceDef(defaultServiceId: Option[String], functions: List[LabelledProductTypeDef])

// Describes top-level function
case class FunctionDef(
  functionName: String,
  arrowTypeDef: ArrowTypeDef,
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
      ArrowTypeDef(args, TypeDefinition(func.returnType)),
      names
    )
  }
}
