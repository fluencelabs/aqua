import {
  ifPropagateErrors,
  registerTestService,
} from "../compiled/examples/ifPropagateErrors.js";

export async function ifPropagateErrorsCall() {
  registerTestService({
    call: (s) => {
      if (s == "fail") return Promise.reject("fail");
      else return Promise.resolve(s);
    },
  });

  return await ifPropagateErrors();
}
