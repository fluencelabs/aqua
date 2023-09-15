package aqua.raw

import aqua.types.ServiceType
import aqua.raw.value.ValueRaw

case class ServiceRaw(
  name: String,
  `type`: ServiceType,
  defaultId: Option[ValueRaw]
) extends RawPart {
  def rawPartType: ServiceType = `type`

  override def rename(s: String): RawPart = copy(name = s)

}
