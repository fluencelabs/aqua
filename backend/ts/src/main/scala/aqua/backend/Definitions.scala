package aqua.backend

import aqua.res.FuncRes
import aqua.types.{ArrowType, OptionType, ProductType, StructType, Type}
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
    case d @ (OptionalType | VoidType | PrimitiveType) =>
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

  def apply(t: Option[Type]): TypeDefinition = t.map(apply).getOrElse(VoidType)

  def apply(t: Type): TypeDefinition = {
    t match {
      case OptionType(t) =>
        OptionalType
      case StructType(name, fields) =>
        StructTypeDef(name, fields.toMap.view.mapValues(TypeDefinition.apply))
      case pt: ProductType =>
        MultiReturnType(pt.toList.map(TypeDefinition.apply))
      case _ => PrimitiveType
    }
  }
}

case object OptionalType extends TypeDefinition { val tag = "optional" }
case object VoidType extends TypeDefinition { val tag = "void" }
case object PrimitiveType extends TypeDefinition { val tag = "primitive" }
case class CallbackType(cDef: CallbackDefinition) extends TypeDefinition { val tag = "callback" }

case class StructTypeDef(name: String, fields: Map[String, TypeDefinition]) extends TypeDefinition {
  val tag = "struct"
}

case class LabelledProductTypeDef(items: List[(String, TypeDefinition)]) extends TypeDefinition {
  val tag = "labelledProduct"
}

case class UnlabelledProductTypeDef(items: List[TypeDefinition]) extends TypeDefinition {
  val tag = "unlabelledProduct"
}

case class ArrowTypeDef(name: String, fields: Map[String, TypeDefinition]) extends TypeDefinition {
  val tag = "arrow"
}

case class MultiReturnType(returnItems: List[TypeDefinition]) extends TypeDefinition {
  val tag = "multiReturn"
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
        VoidType
      case _ =>
        MultiReturnType(returns.map(TypeDefinition.apply))
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

  @tailrec
  def argToDef(name: String, `type`: Type, isOptional: Boolean = false): ArgDefinition = {
    `type` match {
      case OptionType(t) =>
        argToDef(name, t, isOptional = true)
      case a @ ArrowType(_, _) =>
        val callbackDef = CallbackDefinition(a)
        ArgDefinition(name, CallbackType(callbackDef))
      case _ => ArgDefinition(name, if (isOptional) OptionalType else PrimitiveType)
    }
  }

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

// Describes a body of functions, services and callbacks
case class ServiceFunctionDef(
  functionName: String,
  argDefs: List[ArgDefinition],
  returnType: TypeDefinition
)

// Describes service
case class ServiceDef(defaultServiceId: Option[String], functions: List[ServiceFunctionDef])

// Describes top-level function
case class FunctionDef(
  functionName: String,
  returnType: TypeDefinition,
  argDefs: List[ArgDefinition],
  names: NamesConfig
)

object FunctionDef {

  def apply(func: FuncRes): FunctionDef = {
    val args = func.args.map(a => ArgDefinition.argToDef(a.name, a.`type`))

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
      TypeDefinition(func.returnType),
      args,
      names
    )
  }
}
