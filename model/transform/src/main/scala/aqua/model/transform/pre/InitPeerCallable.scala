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

package aqua.model.transform.pre

import aqua.model.OpModel.Tree
import aqua.model.{
  CallModel,
  CallServiceModel,
  LiteralModel,
  OnModel,
  OpModel,
  ValueModel,
  VarModel
}
import aqua.raw.ops.{Call, CallArrowRawTag, OnTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.data.Chain
import aqua.types.Type

sealed trait InitPeerCallable extends PreTransform {

  def onInitPeer: OnModel

  def makeCall(serviceId: ValueRaw, funcName: String, call: Call): RawTag.Tree =
    transform(CallArrowRawTag.service(serviceId, funcName, call).leaf)

  def service(serviceId: ValueRaw): (String, Call) => RawTag.Tree =
    makeCall(serviceId, _, _)
}

// TODO: refactor goThrough into some supertype of VarRaw and VarModel with no properties
case class InitViaRelayCallable(goThrough: Chain[(String, Type)]) extends InitPeerCallable {

  // Get to init user through a relay
  override def transform(op: RawTag.Tree): RawTag.Tree =
    OnTag(
      ValueRaw.InitPeerId,
      goThrough.map { case (n, t) =>
        VarRaw(n, t)
      }
    ).wrap(
      op
    )

  override val onInitPeer: OnModel =
    OnModel(
      LiteralModel.fromRaw(ValueRaw.InitPeerId),
      goThrough.map { case (n, t) =>
        VarModel(n, t, Chain.empty)
      }
    )

}
