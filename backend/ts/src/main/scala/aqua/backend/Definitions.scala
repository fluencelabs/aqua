package aqua.backend

import aqua.types.{ArrowType, OptionType, Type}
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

sealed trait DefType {
  def tag: String
}

object DefType {
  implicit val encodeDefType: Encoder[DefType] = new Encoder[DefType] {

    final def apply(d: DefType): Json = {
      d match {
        case OptionalType | VoidType | PrimitiveType =>
          Json.obj(
            ("tag", Json.fromString(d.tag))
          )
        case CallbackType(cDef) =>
          Json.obj(
            ("tag", Json.fromString(d.tag)),
            ("callback", cDef.asJson)
          )
        case MultiReturnType(returnItems) =>
          Json.obj(
            ("tag", Json.fromString(d.tag)),
            ("returnItems", returnItems.asJson)
          )
      }
    }
  }

  def apply(t: Type): DefType = {
    t match {
      case OptionType(t) =>
        OptionalType
      case _ => PrimitiveType
    }
  }
}

case object OptionalType extends DefType { val tag = "optional" }
case object VoidType extends DefType { val tag = "void" }
case object PrimitiveType extends DefType { val tag = "primitive" }
case class CallbackType(cDef: CallbackDef) extends DefType { val tag = "callback" }
case class MultiReturnType(returnItems: List[DefType]) extends DefType { val tag = "multiReturn" }

case class CallbackDef(argDefs: List[ArgDef], returnType: DefType)

object CallbackDef {

  implicit val encodeCallbackDef: Encoder[CallbackDef] = new Encoder[CallbackDef] {

    final def apply(a: CallbackDef): Json = Json.obj(
      ("argDefs", a.argDefs.asJson),
      ("returnType", a.returnType.asJson)
    )
  }

  def apply(arrow: ArrowType): CallbackDef = {
    val args = arrow.domain.toLabelledList().map(arg => ArgDef.argToDef(arg._1, arg._2))
    val returns = arrow.codomain.toList
    val returnType = returns match {
      case head :: Nil =>
        DefType(head)
      case head :: x =>
        MultiReturnType(returns.map(DefType.apply))
      case Nil =>
        VoidType
    }
    CallbackDef(args, returnType)
  }
}

case class ArgDef(
  name: String,
  argType: DefType
)

object ArgDef {

  def argToDef(name: String, `type`: Type, isOptional: Boolean = false): ArgDef = {
    `type` match {
      case OptionType(t) =>
        argToDef(name, t, isOptional = true)
      case a @ ArrowType(_, _) =>
        val callbackDef = CallbackDef(a)
        ArgDef(name, CallbackType(callbackDef))
      case _ => ArgDef(name, if (isOptional) OptionalType else PrimitiveType)
    }
  }

  implicit val encodeArgDef: Encoder[ArgDef] = new Encoder[ArgDef] {

    final def apply(a: ArgDef): Json = Json.obj(
      ("name", Json.fromString(a.name)),
      ("argType", a.argType.asJson)
    )
  }
}

case class Names(
  relay: String,
  getDataSrv: String,
  callbackSrv: String,
  responseSrv: String,
  responseFnName: String,
  errorHandlingSrv: String,
  errorFnName: String
)

case class FunctionBodyDef(functionName: String, argDefs: List[ArgDef], returnType: DefType)

case class ServiceDef(defaultServiceId: Option[String], functions: List[FunctionBodyDef])

case class FunctionCallDef(
  functionName: String,
  returnType: DefType,
  argDefs: List[ArgDef],
  names: Names
)
