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
  parFunc,
  registerParService,
  testTimeout,
} from "../compiled/examples/par.js";
import { config } from "../config.js";

export async function parCall(relayPeerId: string) {
  let promise = new Promise<string>((resolve, reject) => {
    registerParService({
      call: () => {
        console.log("hello from parservice-id");
        let result = "hello";
        resolve(result);
        return result;
      },
    });
  });

  await parFunc(relayPeerId, (c) => {
    console.log("parFunc. external addresses par: " + c.external_addresses);
  });

  return promise;
}

const relays = config.relays;

export async function testTimeoutCall() {
  return testTimeout([relays[3].peerId, relays[4].peerId]);
}
