import { pipelineStream } from "../../compiled/examples/recursiveStreams/pipeline.js";

export async function pipelineStreamCall(
  init: number,
  target: number,
): Promise<number[]> {
  return await pipelineStream(init, target);
}
