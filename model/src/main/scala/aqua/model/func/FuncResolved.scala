package aqua.model.func

import aqua.model.func.body.OpTag
import aqua.model.transform.{BodyConfig, ForClient}
import aqua.types.ArrowType
import cats.data.Chain
import cats.free.Cofree

case class FuncResolved(name: String, func: FuncCallable) {

  def forClient(conf: BodyConfig): Cofree[Chain, OpTag] =
    ForClient(this, conf)

  def arrowType: ArrowType =
    ArrowType(
      func.args.types,
      func.ret.map(_.`type`)
    )
}
