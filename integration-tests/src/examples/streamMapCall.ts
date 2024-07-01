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
  testGetFunc, testGetStreamFunc, testKeysFunc, testKeysStreamFunc, testContainsFunc,
  testForFunc, testParSeqMap, testForTupleFunc, testInsertMapFromFunc
} from "../compiled/examples/streamMap.js";
import { config } from "../config.js";

const relays = config.relays;

export async function testGetFuncCall() {
  return testGetFunc();
}

export async function testGetStreamFuncCall() {
  return testGetStreamFunc();
}

export async function testKeysFuncCall() {
  return testKeysFunc();
}

export async function testKeysStreamFuncCall() {
  return testKeysStreamFunc();
}

export async function testContainsFuncCall() {
  return testContainsFunc();
}

export async function testForFuncCall() {
  return testForFunc();
}

export async function testForTupleFuncCall() {
  return testForTupleFunc();
}

export async function testParSeqMapCall() {
    return testParSeqMap(relays[3].peerId, relays[4].peerId, relays[5].peerId)
}

export async function testInsertMapFromFuncCall() {
    return testInsertMapFromFunc()
}

