import {
  simpleTest,
  complexTest
} from "../compiled/examples/closureStreamScopes.js";

export async function complexCall(): Promise<string[]> {
  return complexTest();
}

export async function simpleCall(): Promise<[string[], string[]]> {
  return simpleTest();
}
