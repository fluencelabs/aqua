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
  retrieve_records,
  modify_stream,
  registerTestService,
  registerStreamService,
  lng280Bug,
  lng280BugWithFor,
  lng280BugWithForAnonStream,
  returnDerivedStream,
  lng280BugWithForEmptyStreamFunc
} from "../compiled/examples/streamArgs.js";

export async function streamArgsCall() {
  registerTestService({
    get_records: (key) => {
      return [key, key];
    },
  });

  return await retrieve_records("peer_id");
}

export async function modifyStreamCall(arg: string[]) {
  return await modify_stream(arg);
}

export async function lng280BugCall(): Promise<string[]> {
  return lng280Bug()
}

export async function lng280BugWithForCall(): Promise<string[]> {
  return lng280BugWithFor()
}

export async function lng280BugWithForAnonStreamCall(): Promise<number[][]> {
  let storage: number[][] = []
  registerStreamService({
    store: (numbers, n) => {
      numbers.push(n)
      storage.push(numbers)
    },
  });
  await lng280BugWithForAnonStream()

  return storage
}

export async function lng280BugWithForEmptyStreamFuncCall(): Promise<number[][]> {
  let storage: number[][] = []
  registerStreamService({
    store: (numbers, n) => {
      numbers.push(n)
      storage.push(numbers)
    },
  });
  await lng280BugWithForEmptyStreamFunc()

  return storage
}

export async function returnDerivedStreamCall(): Promise<number[]> {
  return returnDerivedStream()
}
