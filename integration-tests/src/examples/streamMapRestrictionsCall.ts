import { streamMapRes } from "../compiled/examples/streamMapRestriction.js";

export async function streamMapResCall(): Promise<any> {
  return await streamMapRes(["a", "b", "c"]);
}
