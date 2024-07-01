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
  test1,
  test2,
  testI16,
  testI32,
  testI64,
  testU64,
} from "../compiled/examples/math.js";

export async function mathTest1Call(): Promise<number> {
  return await test1();
}

export async function mathTest2Call(): Promise<number> {
  return await test2();
}

export async function mathTestI16Call(peer: string): Promise<number[]> {
  return await testI16(peer);
}

export async function mathTestI32Call(peer: string): Promise<number[]> {
  return await testI32(peer);
}

export async function mathTestI64Call(peer: string): Promise<number[]> {
  return await testI64(peer);
}

export async function mathTestU64Call(peer: string): Promise<number[]> {
  return await testU64(peer);
}
