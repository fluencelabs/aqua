import { get_results, registerOpA } from "../compiled/examples/pushToStream.js";

export async function pushToStreamCall() {
  registerOpA({
    get_str: () => {
      return "get_string";
    },
  });

  return await get_results();
}
