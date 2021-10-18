package aqua.backend

import aqua.types.{ArrowType, OptionType, ProductType, Type}
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

import scala.annotation.tailrec

sealed trait TypeDefinition {
  def tag: String
}

object TypeDefinition {

  implicit val encodeDefType: Encoder[TypeDefinition] = {
    case d@(OptionalType | VoidType | PrimitiveType) =>
      Json.obj(
        ("tag", Json.fromString(d.tag))
      )
    case d@CallbackType(cDef) =>
      Json.obj(
        ("tag", Json.fromString(d.tag)),
        ("callback", cDef.asJson)
      )
    case d@MultiReturnType(returnItems) =>
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

case class MultiReturnType(returnItems: List[TypeDefinition]) extends TypeDefinition {
  val tag = "multiReturn"
}

case class CallbackDefinition(argDefs: List[ArgDefinition], returnType: TypeDefinition)

object CallbackDefinition {

  implicit val encodeCallbackDef: Encoder[CallbackDefinition] = (cbDef: CallbackDefinition) => Json.obj(
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

  implicit val encodeArgDef: Encoder[ArgDefinition] = (a: ArgDefinition) => Json.obj(
    ("name", Json.fromString(a.name)),
    ("argType", a.argType.asJson)
  )
}

// Names of services and functions that are used in air
case class Names(
  relay: String,
  getDataSrv: String,
  callbackSrv: String,
  responseSrv: String,
  responseFnName: String,
  errorHandlingSrv: String,
  errorFnName: String
)

case class FunctionBodyDef(
  functionName: String,
  argDefs: List[ArgDefinition],
  returnType: TypeDefinition
)

case class ServiceDef(defaultServiceId: Option[String], functions: List[FunctionBodyDef])

case class FunctionCallDef(
  functionName: String,
  returnType: TypeDefinition,
  argDefs: List[ArgDefinition],
  names: Names
)
