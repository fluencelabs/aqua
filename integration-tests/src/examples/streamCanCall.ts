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
