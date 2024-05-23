import {
  testGetFunc, testGetStreamFunc
} from "../compiled/examples/streamMap.js";

export async function testGetFuncCall() {
  return testGetFunc();
}

export async function testGetStreamFuncCall() {
  return testGetStreamFunc();
}

