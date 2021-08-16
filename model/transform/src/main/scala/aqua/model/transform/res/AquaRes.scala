package aqua.model.transform.res

import aqua.model.AquaContext
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.transform.res._
import cats.data.Chain

case class AquaRes(funcs: Chain[FuncRes], services: Chain[ServiceRes]) {
  def isEmpty: Boolean = funcs.isEmpty && services.isEmpty
}

object AquaRes {

  def fromContext(ctx: AquaContext, conf: TransformConfig): AquaRes =
    AquaRes(
      funcs = Chain.fromSeq(ctx.funcs.values.toSeq).map(Transform.fn(_, conf)),
      services = Chain.fromSeq(ctx.services.values.toSeq).map(ServiceRes.fromModel(_))
    )
}
