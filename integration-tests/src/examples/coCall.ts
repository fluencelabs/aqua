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

import { parFunc } from "../compiled/examples/par.js";
import { registerCoService } from "../compiled/examples/co.js";
import { relay1 } from "../__test__/examples.spec.js";

export async function coCall(): Promise<string[]> {
  registerCoService({
    call: () => {
      return "hello";
    },
  });

  return new Promise<string[]>((resolve, reject) => {
    parFunc(
      relay1.peerId,
      (c) => {
        resolve(c.external_addresses);
      },
      { ttl: 60000 },
    );
  });
}
