import { range } from "../../compiled/examples/fork/range.js";

export async function rangeCall(a: number, b: number): Promise<number[]> {
  return await range(a, b);
}
