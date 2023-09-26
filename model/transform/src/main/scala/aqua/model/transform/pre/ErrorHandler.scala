package aqua.model.transform.pre

import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.raw.ops.{Call, CallArrowRawTag, RawTag}
import aqua.types.LiteralType

import cats.Eval
import cats.data.Chain
import cats.free.Cofree

trait ErrorHandler {
  def handleError: RawTag.Tree
}

case class CallbackErrorHandler(
  serviceId: ValueRaw,
  funcName: String
) extends ErrorHandler {

  override def handleError: RawTag.Tree = {
    val call = Call(
      args = ValueRaw.error :: LiteralRaw.number(0) :: Nil,
      exportTo = Nil
    )

    CallArrowRawTag.service(serviceId, funcName, call).leaf
  }
}
