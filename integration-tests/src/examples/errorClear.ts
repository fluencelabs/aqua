import {
  errorClearTest,
  registerFailService,
} from "../compiled/examples/errorClear.js";

export async function errorClearCall(node: string) {
  registerFailService({
    call: (s: string) => {
      if (s === "fail") return Promise.reject(s);
      else return Promise.resolve(s);
    },
  });

  return await errorClearTest(node);
}
