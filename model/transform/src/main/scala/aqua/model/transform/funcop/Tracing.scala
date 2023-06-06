package aqua.model.transform.funcop

import cats.data.Chain

import cats.Eval

import aqua.model.{
  CallModel,
  CallServiceModel,
  LiteralModel,
  MetaModel,
  OpModel,
  SeqModel,
  ValueModel
}
import aqua.model.transform.pre.InitPeerCallable
import aqua.model.ParModel
import aqua.model.DetachModel

final case class Tracing(
  enabled: Boolean,
  serviceId: String,
  serviceFuncName: String,
  initCallable: InitPeerCallable
) extends OpTransform {
  import Tracing.*

  private def getChild(children: Chain[OpModel.Tree]): OpModel.Tree =
    children.headOption
      .filter(_ => children.length == 1)
      .getOrElse(
        SeqModel.wrap(children.toList: _*)
      )

  private def traceCall(arrowName: String, event: Event): OpModel.Tree =
    CallServiceModel(
      LiteralModel.liftString(serviceId),
      serviceFuncName,
      CallModel(
        args = List(LiteralModel.liftString(arrowName), event.toArg),
        exportTo = Nil
      )
    ).leaf

  override def folder: OpTransform.OpFolder = {
    case (MetaModel.CallArrowModel(arrowName), children) =>
      val child = getChild(children)

      Eval.now(
        if (!enabled) child
        else
          SeqModel.wrap(
            ParModel.wrap(
              initCallable.onInitPeer.wrap(
                traceCall(arrowName, Event.Enter)
              ),
              child
            ),
            DetachModel.wrap(
              initCallable.onInitPeer.wrap(
                traceCall(arrowName, Event.Exit)
              )
            )
          )
      )
  }
}

object Tracing {

  enum Event {
    case Enter, Exit

    def toArg: ValueModel = LiteralModel.liftString(this match {
      case Enter => "enter"
      case Exit => "exit"
    })
  }
}
