package aqua.backend

import aqua.types.{ArrowType, OptionType}

case class ReturnType(isVoid: Boolean, isOptional: Boolean)

case class CallbackDef(argNames: List[String], returnType: ReturnType)

object CallbackDef {
  def apply(arrow: ArrowType): CallbackDef = {
    val args = arrow.codomain.toLabelledList().map(a => a._1)
    val returns = arrow.domain.toList
    val returnType = returns match {
      case head :: Nil =>
        head match {
          case OptionType(t) =>
            ReturnType(isVoid = false, isOptional = true)
          case _ => ReturnType(isVoid = false, isOptional = false)
        }
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

case class Names(
  relay: String,
  getDataSrv: String,
  callbackSrv: String,
  responseSrv: String,
  responseFnName: String,
  errorHandlingSrv: String,
  errorFnName: String
)

case class FunctionBodyDef(functionName: String, argNames: List[String], returnType: ReturnType)

case class ServiceDef(functions: List[FunctionBodyDef])

case class FunctionCallDef(
  functionName: String,
  isVoid: Boolean,
  args: List[ArgDef],
  names: Names
)
