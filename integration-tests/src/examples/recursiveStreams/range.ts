import { range } from "../../compiled/examples/recursiveStreams/range.js";

export async function rangeCall(a: number, b: number): Promise<number[]> {
  return await range(a, b);
}
