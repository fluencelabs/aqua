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
  forBug499,
  iterateAndPrint,
  iterateAndPrintParallel,
} from "../compiled/examples/fold.js";

export async function foldCall(relayPeerId: string) {
  await iterateAndPrint([relayPeerId], { ttl: 10000 });

  return new Promise<string[]>((resolve, reject) => {
    iterateAndPrintParallel(
      [relayPeerId],
      (c) => {
        console.log(
          "iterateAndPrintParallel. external addresses: " +
            c.external_addresses,
        );
        resolve(c.external_addresses);
      },
      { ttl: 10000 },
    );
  });
}

export async function foldBug499Call(): Promise<number[]> {
  return forBug499();
}
