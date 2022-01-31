package aqua.model.inline

import aqua.model.{OpModel, ParModel}

// TODO may not be needed there
private[inline] object Inline {

  def parDesugarPrefix(ops: List[OpModel.Tree]): Option[OpModel.Tree] = ops match {
    case Nil => None
    case x :: Nil => Option(x)
    case _ => Option(ParModel.wrap(ops: _*))
  }

  def parDesugarPrefixOpt(ops: Option[OpModel.Tree]*): Option[OpModel.Tree] =
    parDesugarPrefix(ops.toList.flatten)
}
