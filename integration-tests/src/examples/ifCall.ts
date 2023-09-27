import {
  bugLNG69,
  ifCorrectXorWrap,
  ifElseCall,
  ifElseNumCall,
} from "../compiled/examples/if.js";

export async function ifCall() {
  await ifElseCall(false);
  await ifElseCall(true);

  await ifElseNumCall(1);
  await ifElseNumCall(5);
}

export async function ifWrapCall(node: string) {
  return ifCorrectXorWrap(node);
}

export async function bugNG69Call(node: string): Promise<boolean> {
  return bugLNG69(node);
}
