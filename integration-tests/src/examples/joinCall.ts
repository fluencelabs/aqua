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

import {
  joinIdx,
  joinIdxLocal,
  joinIdxRelay,
} from "../compiled/examples/join.js";
import { config } from "../config.js";

const relays = config.relays;

export async function joinIdxCall(relayPeerId: string) {
  // join.aqua

  return await joinIdx(
    1,
    [relayPeerId, relays[2].peerId, relays[4].peerId, relays[5].peerId],
    { ttl: 10000 },
  );
}

export async function joinIdxLocalCall(relayPeerId: string) {
  // join.aqua

  return await joinIdxLocal(2, [
    relayPeerId,
    relays[2].peerId,
    relays[4].peerId,
  ]);
}

export async function joinIdxRelayCall(relayPeerId: string) {
  // join.aqua

  return await joinIdxRelay(
    2,
    [relayPeerId, relays[2].peerId, relays[4].peerId],
    { ttl: 30000 },
  );
}
