/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.raw

import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ScalarType, Type}

case class ConstantRaw(name: String, value: ValueRaw, allowOverrides: Boolean) extends RawPart {
  override def rename(s: String): RawPart = copy(name = s)

  override def rawPartType: Type = value.`type`

  def addAbilityName(s: String): RawPart = this
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
      relayVarName.fold(ValueRaw.InitPeerId)(r => VarRaw(r, ScalarType.string)),
      false
    )

  def defaultConstants(relayVarName: Option[String] = None): List[ConstantRaw] =
    hostPeerId(
      relayVarName
    ) :: initPeerId :: particleTtl :: particleTimestamp :: nil :: lastError :: Nil
}
