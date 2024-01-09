import {
  yesNoStream,
  registerYesNoService,
} from "../../compiled/examples/recursiveStreams/yesNo.js";

export async function yesNoStreamCall(limit: number): Promise<string[]> {
  let i = 1;
  registerYesNoService({
    get: () => {
      i += 1;
      return i > limit ? "no" : "yes";
    },
  });

  return await yesNoStream();
}
