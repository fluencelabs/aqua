import {
  simpleTest,
  complexTest
} from "../compiled/examples/closureStreamScopes.js";

export async function complexStreamScopeCall(): Promise<string[]> {
  return complexTest();
}

export async function simpleStreamScopeCall(): Promise<[string[], string[]]> {
  return simpleTest();
}
