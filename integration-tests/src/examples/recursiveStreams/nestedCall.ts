import { nested } from "../../compiled/examples/recursiveStreams/nested.js";

export async function nestedCall(n: number): Promise<number[]> {
  return await nested(n);
}
