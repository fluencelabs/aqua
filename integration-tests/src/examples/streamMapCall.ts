import {
  testGetFunc, testGetStreamFunc, testKeysFunc, testContainsFunc, testForFunc
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

export async function testContainsFuncCall() {
  return testContainsFunc();
}

export async function testForFuncCall() {
  return testForFunc();
}

