import {
  recursiveStream,
  registerYesNoService,
} from "../compiled/examples/recursiveStreams.js";

export async function recursiveStreamsCall(): Promise<[string[], string[]]> {
  let i = 0;
  registerYesNoService({
    get: () => {
      i++;
      if (i > 3) {
        console.log("return no");
        return "no";
      } else {
        console.log("return yes");
        return "yes";
      }
    },
  });

  return await recursiveStream();
}
