package aqua.model.transform.res

import aqua.model.AquaContext
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.transform.res._
import cats.data.Chain

case class AquaRes(funcs: Chain[FuncRes], services: Chain[ServiceRes]) {
  def isEmpty: Boolean = funcs.isEmpty && services.isEmpty
}

object AquaRes {
  private val blank = AquaRes(Chain.nil, Chain.nil)

  def fromContext(ctx: AquaContext, conf: TransformConfig): AquaRes =
    ctx.exports
      .map(ex =>
        AquaRes(
          funcs = Chain.fromSeq(ex.funcs.values.toSeq).map(Transform.fn(_, conf)),
          services = Chain.fromSeq(ex.services.values.toSeq).map(ServiceRes.fromModel(_))
        )
      )
      .getOrElse(blank)

}
