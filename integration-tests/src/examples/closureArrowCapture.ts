import {
  test,
  registerTestServiceClosureArrowCapture,
} from "../compiled/examples/closureArrowCapture.js";

export async function closureArrowCaptureCall(s: string) {
  registerTestServiceClosureArrowCapture("test-service", {
    call: (s: string) => {
      return "call: " + s;
    },
  });

  return await test(s);
}
