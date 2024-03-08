import {
  simpleTest,
  complexTest
} from "../compiled/examples/closureStreamScopes.js";

export async function complexStreamScopeCall(): Promise<[string[], string[]]> {
  return complexTest();
}

export async function simpleStreamScopeCall(): Promise<[string[]]> {
  return simpleTest();
}
