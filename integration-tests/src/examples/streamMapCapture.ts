import {
  testStreamMapCaptureSimple,
  testStreamMapCaptureReturn,
} from "../compiled/examples/streamMapCapture.js";

export async function streamMapCaptureSimpleCall() {
  return await testStreamMapCaptureSimple();
}

export async function streamMapCaptureReturnCall() {
  return await testStreamMapCaptureReturn();
}
