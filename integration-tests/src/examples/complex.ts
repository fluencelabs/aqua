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

import { doStuff, registerTestS } from "../compiled/examples/complex.js";

export async function complexCall(selfPeerId: string, relayPeerId: string) {
  registerTestS({
    t: (arg0) => {
      return arg0;
    },
    multiline: (a, b, c) => {
      return b;
    },
  });

  return await doStuff(
    relayPeerId,
    selfPeerId,
    true,
    true,
    ["1", "2"],
    ["3", "4"],
    "some str",
  );
}
