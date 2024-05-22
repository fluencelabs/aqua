package aqua.model.transform.funcop

import aqua.model.*
import aqua.model.transform.pre.InitPeerCallable
import aqua.model.ParModel
import aqua.model.DetachModel
import aqua.model.transform.TransformConfig.TracingConfig

import cats.data.Chain
import cats.Eval
import aqua.types.ScalarType

final case class Tracing(
  enabledConfig: Option[TracingConfig],
  initCallable: InitPeerCallable
) extends OpTransform {
  import Tracing.*

  private def getChild(children: Chain[OpModel.Tree]): OpModel.Tree =
    children.headOption
      .filter(_ => children.length == 1)
      .getOrElse(
        SeqModel.wrap(children.toList*)
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
                 xor:
                   seq:
                     <call-arrow-code>
                     detach: call tracing exit
                   seq:
                     call tracing error exit
                     rethrow error
             */
            SeqModel.wrap(
              DetachModel.wrap(
                initCallable.onInitPeer.wrap(
                  traceCall(Event.Enter)
                )
              ),
              XorModel.wrap(
                SeqModel.wrap(
                  child,
                  DetachModel.wrap(
                    initCallable.onInitPeer.wrap(
                      traceCall(Event.Exit)
                    )
                  )
                ),
                SeqModel.wrap(
                  /**
                   * NOTE: Here we don't wrap trace call
                   * with detach because Aqua VM ignores
                   * it if it is detached.
                   */
                  initCallable.onInitPeer.wrap(
                    traceCall(Event.ErrorExit)
                  ),
                  FailModel(ValueModel.error).leaf
                )
              )
            )
          )
      )
  }
}

object Tracing {

  enum Event {
    case Enter, Exit, ErrorExit

    def toArg(suffix: String = ""): ValueModel = LiteralModel.quote((this match {
      case Enter => "enter"
      case Exit => "exit"
      case ErrorExit => "exit with error"
    }) + suffix)
  }

  private def traceCallModel(config: TracingConfig, arrowName: String)(
    event: Event
  ): OpModel.Tree = {
    val serviceCall = (msg: ValueModel) =>
      CallServiceModel(
        LiteralModel.quote(config.serviceId),
        config.serviceFuncName,
        CallModel(
          args = List(LiteralModel.quote(arrowName), msg),
          exportTo = Nil
        )
      ).leaf

    event match {
      case Event.ErrorExit =>
        val errorName = "-return-error-msg-"

        RestrictionModel(
          errorName,
          ScalarType.string
        ).wrap(
          CallServiceModel(
            LiteralModel.quote("op"),
            "concat_strings",
            CallModel(
              args = List(event.toArg(": "), ValueModel.lastErrorMessage),
              exportTo = List(CallModel.Export(errorName, ScalarType.string))
            )
          ).leaf,
          serviceCall(VarModel(errorName, ScalarType.string))
        )
      case _ => serviceCall(event.toArg())
    }
  }
}
