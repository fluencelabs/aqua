import {
  callReturnedArrow,
  callReturnedChainArrow,
} from "../compiled/examples/returnArrow.js";

export async function returnArrowCall(): Promise<[string, string]> {
  return await callReturnedArrow("arg for func ", "arg for closure ");
}

export async function returnArrowChainCall(): Promise<
  [string, string, string, string, string, string, string, string]
> {
  return await callReturnedChainArrow("arg for func1 ", "arg for func2 ");
}
