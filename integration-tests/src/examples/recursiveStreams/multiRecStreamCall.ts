import {
  multiRecStream,
  registerTestServiceMultiRec,
} from "../../compiled/examples/recursiveStreams/multiRec.js";

export async function multiRecStreamCall(
  init: number,
  target: number,
  handle: (i: number) => number[],
): Promise<number[]> {
  registerTestServiceMultiRec({ handle });

  return await multiRecStream(init, target);
}
