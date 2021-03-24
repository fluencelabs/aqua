package aqua.model

import cats.data.Chain

sealed trait ValueModel

case class LiteralModel(value: String) extends ValueModel
case object InitPeerIdModel extends ValueModel

sealed trait LambdaModel
case object IntoArrayModel extends LambdaModel
case class IntoFieldModel(field: String) extends LambdaModel

case class VarModel(name: String, lambda: Chain[LambdaModel] = Chain.empty) extends ValueModel
