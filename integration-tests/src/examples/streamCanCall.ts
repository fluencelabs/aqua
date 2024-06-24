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
  accumRes,
  bugLNG63,
  bugLNG63_2,
  bugLNG63_3,
} from "../compiled/examples/streamCan.js";

export async function streamCanCall() {
  return await accumRes();
}

export async function bugLNG63Call(): Promise<string> {
  return await bugLNG63();
}

export async function bugLNG63_2Call(): Promise<[string, string[], string[]]> {
  return await bugLNG63_2();
}

export async function bugLNG63_3Call(): Promise<[string, number, number[]]> {
  return await bugLNG63_3();
}
