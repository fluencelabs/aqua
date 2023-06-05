package aqua.model.transform.funcop

import cats.data.Chain

import cats.Eval

import aqua.model.{MetaModel, OpModel, SeqModel}

final case class Tracing(
  enabled: Boolean
) extends OpTransform {

  override def folder: OpTransform.OpFolder = {
    case (MetaModel.CallArrowModel(arrowName), children) =>
      Eval.now(
        children.headOption
          .filter(_ => children.length == 1)
          .getOrElse(
            SeqModel.wrap(children.toList: _*)
          )
      )
  }
}
