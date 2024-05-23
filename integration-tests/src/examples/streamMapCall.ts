import {
  testGetFunc, testGetStreamFunc, testKeysFunc
} from "../compiled/examples/streamMap.js";

export async function testGetFuncCall() {
  return testGetFunc();
}

export async function testGetStreamFuncCall() {
  return testGetStreamFunc();
}

export async function testKeysFuncCall() {
  return testKeysFunc();
}

