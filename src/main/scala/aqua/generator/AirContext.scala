package aqua.generator

import DataView.InitPeerId

case class AirContext(
  data: Map[String, DataView] = Map.empty,
  arrows: Map[String, ArrowCallable] = Map.empty,
  peerId: DataView = InitPeerId,
  vars: Set[String] = Set.empty,
  instrCounter: Int = 0
) {

  def mergePar(other: AirContext): AirContext =
    copy(
      instrCounter = instrCounter max other.instrCounter,
      data = data ++ other.data,
      vars = vars ++ other.vars
    )

  def incr: AirContext = copy(instrCounter = instrCounter + 1)
}
