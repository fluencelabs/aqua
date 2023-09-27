import {
  use_name2,
  registerDTGetter,
} from "../compiled/examples/streamResults.js";

export async function streamResultsCall() {
  registerDTGetter({
    get_dt: (args0) => {
      return {
        field: args0,
      };
    },
  });

  return await use_name2("new_name");
}
