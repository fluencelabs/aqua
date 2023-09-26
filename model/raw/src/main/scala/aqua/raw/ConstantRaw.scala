package aqua.raw

import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ScalarType, Type}

case class ConstantRaw(name: String, value: ValueRaw, allowOverrides: Boolean) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  override def rawPartType: Type = value.`type`
}

object ConstantRaw {

  val initPeerId: ConstantRaw =
    ConstantRaw(
      "INIT_PEER_ID",
      ValueRaw.InitPeerId,
      false
    )

  val particleTtl: ConstantRaw =
    ConstantRaw(
      "PARTICLE_TTL",
      ValueRaw.ParticleTtl,
      false
    )

  val particleTimestamp: ConstantRaw =
    ConstantRaw(
      "PARTICLE_TIMESTAMP",
      ValueRaw.ParticleTimestamp,
      false
    )

  val nil: ConstantRaw =
    ConstantRaw(
      "nil",
      ValueRaw.Nil,
      false
    )

  val lastError: ConstantRaw =
    ConstantRaw(
      "LAST_ERROR",
      ValueRaw.error,
      false
    )

  // Host peer id holds %init_peer_id% in case Aqua is not compiled to be executed behind a relay,
  // or relay's variable otherwise
  def hostPeerId(relayVarName: Option[String]): ConstantRaw =
    ConstantRaw(
      "HOST_PEER_ID",
      relayVarName.fold[ValueRaw](ValueRaw.InitPeerId)(r => VarRaw(r, ScalarType.string)),
      false
    )

  def defaultConstants(relayVarName: Option[String]): List[ConstantRaw] =
    hostPeerId(
      relayVarName
    ) :: initPeerId :: particleTtl :: particleTimestamp :: nil :: lastError :: Nil
}
