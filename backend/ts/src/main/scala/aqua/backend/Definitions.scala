package aqua.backend

import aqua.types.{ArrowType, OptionType, Type}
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

import scala.annotation.tailrec

case class ReturnType(isVoid: Boolean, isOptional: Boolean)

object ReturnType {

  def apply(t: Type): ReturnType = {
    t match {
      case OptionType(t) =>
        ReturnType(isVoid = false, isOptional = true)
      case _ => ReturnType(isVoid = false, isOptional = false)
    }
  }
}

case class CallbackDef(argDefs: List[ArgDef], returnType: ReturnType)

object CallbackDef {

  implicit val encodeCallbackDef: Encoder[CallbackDef] = new Encoder[CallbackDef] {

    final def apply(a: CallbackDef): Json = Json.obj(
      ("argDefs", a.argDefs.asJson),
      ("returnType", a.returnType.asJson)
    )
  }

  def apply(arrow: ArrowType): CallbackDef = {
    val args = arrow.codomain.toLabelledList().map(arg => ArgDef.argToDef(arg._1, arg._2))
    val returns = arrow.domain.toList
    val returnType = returns match {
      case head :: Nil =>
        ReturnType(head)
      case head :: x =>
        ReturnType(isVoid = false, isOptional = false)
      case Nil =>
        ReturnType(isVoid = true, isOptional = false)
    }
    CallbackDef(args, returnType)
  }
}

case class ArgDef(
  name: String,
  isOptional: Boolean,
  callbackDef: Option[CallbackDef]
)

object ArgDef {

  def argToDef(name: String, `type`: Type, isOptional: Boolean = false): ArgDef = {
    `type` match {
      case OptionType(t) =>
        argToDef(name, t, isOptional = true)
      case a @ ArrowType(_, _) =>
        val callbackDef = CallbackDef(a)
        ArgDef(name, isOptional, Some(callbackDef))
      case _ => ArgDef(name, isOptional, None)
    }
  }

  implicit val encodeArgDef: Encoder[ArgDef] = new Encoder[ArgDef] {

    final def apply(a: ArgDef): Json = Json.obj(
      ("name", Json.fromString(a.name)),
      ("isOptional", Json.fromBoolean(a.isOptional)),
      ("callbackDef", a.callbackDef.asJson)
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

case class FunctionBodyDef(functionName: String, argDefs: List[ArgDef], returnType: ReturnType)

case class ServiceDef(defaultServiceId: Option[String], functions: List[FunctionBodyDef])

case class FunctionCallDef(
  functionName: String,
  returnType: ReturnType,
  argDefs: List[ArgDef],
  names: Names
)
