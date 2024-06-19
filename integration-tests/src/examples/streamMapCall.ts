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

