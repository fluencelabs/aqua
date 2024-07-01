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
  multiRecStream,
  registerTestServiceMultiRec,
} from "../../compiled/examples/recursiveStreams/multiRec.js";

export async function multiRecStreamCall(
  init: number,
  target: number,
  handle: (i: number) => number[],
): Promise<number[]> {
  registerTestServiceMultiRec({ handle });

  return await multiRecStream(init, target);
}
