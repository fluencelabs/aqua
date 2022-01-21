package aqua.res

import cats.data.Chain

// TODO: doc
case class AquaRes(funcs: Chain[FuncRes], services: Chain[ServiceRes]) {
  def isEmpty: Boolean = funcs.isEmpty && services.isEmpty
}

object AquaRes {
  private val blank = AquaRes(Chain.nil, Chain.nil)
}
