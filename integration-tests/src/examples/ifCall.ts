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
  bugLNG69,
  ifCorrectXorWrap,
  ifElseCall,
  ifElseNumCall,
} from "../compiled/examples/if.js";

export async function ifCall() {
  await ifElseCall(false);
  await ifElseCall(true);

  await ifElseNumCall(1);
  await ifElseNumCall(5);
}

export async function ifWrapCall(node: string) {
  return ifCorrectXorWrap(node);
}

export async function bugNG69Call(node: string): Promise<boolean> {
  return bugLNG69(node);
}
