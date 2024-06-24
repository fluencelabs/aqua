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

import { bugLng79, registerSer } from "../compiled/examples/canon.js";

export async function bugLng79Call(
  pid: string,
  relay: string,
): Promise<number> {
  registerSer({
    getRecord: () => {
      return { peer_id: pid, relay_id: [relay] };
    },
  });
  return await bugLng79((s) => {
    console.log(s);
  });
}
