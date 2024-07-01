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
  passFunctionAsArg,
  reproArgsBug426,
} from "../compiled/examples/callArrow.js";

export async function callArrowCall(relayPeerId: string): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    passFunctionAsArg(
      relayPeerId,
      "callArrow call",
      (a: string) => {
        let result = "Hello, " + a + "!";
        console.log(result);
        resolve(result);
        return result;
      },
      { ttl: 10000 },
    );
  });
}

export async function reproArgsBug426Call(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    reproArgsBug426((a: string) => {
      resolve(a);
    }, "privet");
  });
}
