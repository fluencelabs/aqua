package aqua.model

import aqua.model.body.OpTag
import aqua.model.transform.{BodyConfig, ForClient}
import cats.data.Chain
import cats.free.Cofree

case class FuncResolved(name: String, func: FuncCallable) {
  def forClient(conf: BodyConfig): Cofree[Chain, OpTag] = ForClient(this, conf)
}
