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
  bugLNG60,
  create_client_util,
  registerAquaDHT,
} from "../compiled/examples/passArgs.js";

export async function passArgsCall() {
  registerAquaDHT({
    put_host_value: (args0, args1) => {
      return args0 + args1;
    },
  });

  return await create_client_util("sid");
}

export async function bugLNG60Call(relayPeerId: string): Promise<boolean> {
  return bugLNG60(relayPeerId, { ttl: 10000 });
}
