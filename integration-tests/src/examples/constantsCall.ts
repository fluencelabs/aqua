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
  callConstant,
  registerGetter,
  timestampAndTtl,
} from "../compiled/examples/constants.js";

export async function constantsCall(): Promise<string[]> {
  registerGetter({
    createStr: (arg0) => {
      return "" + arg0;
    },
  });

  return await callConstant();
}

export async function particleTtlAndTimestampCall(
  ttl: number,
): Promise<[number, number]> {
  return await timestampAndTtl({ ttl: ttl });
}
