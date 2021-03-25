package aqua.model

import cats.data.Chain

sealed trait ValueModel {
  def resolveWith(map: Map[String, ValueModel]): ValueModel = this
}

case class LiteralModel(value: String) extends ValueModel
case object InitPeerIdModel extends ValueModel

sealed trait LambdaModel
case object IntoArrayModel extends LambdaModel
case class IntoFieldModel(field: String) extends LambdaModel

case class VarModel(name: String, lambda: Chain[LambdaModel] = Chain.empty) extends ValueModel {
  def deriveFrom(vm: VarModel): VarModel = vm.copy(lambda = vm.lambda ++ lambda)

  override def resolveWith(map: Map[String, ValueModel]): ValueModel = map.get(name) match {
    case Some(vv: VarModel) => deriveFrom(vv)
    case Some(vv) => vv // TODO check that lambda is empty, otherwise error
    case None => this // Should not happen
  }
}
