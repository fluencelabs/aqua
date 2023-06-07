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
import aqua.model.transform.TransformConfig.TracingConfig

final case class Tracing(
  enabledConfig: Option[TracingConfig],
  initCallable: InitPeerCallable
) extends OpTransform {
  import Tracing.*

  private def getChild(children: Chain[OpModel.Tree]): OpModel.Tree =
    children.headOption
      .filter(_ => children.length == 1)
      .getOrElse(
        SeqModel.wrap(children.toList: _*)
      )

  override def folder: OpTransform.OpFolder = {
    case (MetaModel.CallArrowModel(arrowName), children) =>
      val child = getChild(children)

      Eval.now(
        enabledConfig
          .map(traceCallModel(_, arrowName))
          .fold(child)(traceCall =>
            /* seq:
                 detach: call tracing enter
                 <call-arrow-code>
                 detach: call tracing exit */
            SeqModel.wrap(
              DetachModel.wrap(
                initCallable.onInitPeer.wrap(
                  traceCall(Event.Enter)
                )
              ),
              child,
              DetachModel.wrap(
                initCallable.onInitPeer.wrap(
                  traceCall(Event.Exit)
                )
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

  def traceCallModel(config: TracingConfig, arrowName: String)(
    event: Event
  ): OpModel.Tree =
    CallServiceModel(
      LiteralModel.liftString(config.serviceId),
      config.serviceFuncName,
      CallModel(
        args = List(LiteralModel.liftString(arrowName), event.toArg),
        exportTo = Nil
      )
    ).leaf
}
