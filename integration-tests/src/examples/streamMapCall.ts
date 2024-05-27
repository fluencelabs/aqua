import {
  testGetFunc, testGetStreamFunc, testKeysFunc, testContainsFunc
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

