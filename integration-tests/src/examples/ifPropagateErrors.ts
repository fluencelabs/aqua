import {
  ifPropagateErrors,
  registerTestService,
} from "../compiled/examples/ifPropagateErrors.js";

export async function ifPropagateErrorsCall() {
  registerTestService({
    call: (s) => {
      if (s.startsWith("fail")) return Promise.reject(s);
      else return Promise.resolve(s);
    },
  });

  return await ifPropagateErrors();
}
