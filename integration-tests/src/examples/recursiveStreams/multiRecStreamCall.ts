import {
  multiRecStream,
  registerTestService,
} from "../../compiled/examples/recursiveStreams/multiRec.js";

export async function multiRecStreamCall(
  init: number,
  target: number,
  handle: (i: number) => number[],
): Promise<number[]> {
  registerTestService({ handle });

  return await multiRecStream(init, target);
}
