import {
  test,
  registerTestService,
} from "../compiled/examples/closureArrowCapture.js";

export async function closureArrowCaptureCall(s: string) {
  registerTestService("test-service", {
    call: (s: string) => {
      return "call: " + s;
    },
  });

  return await test(s);
}
