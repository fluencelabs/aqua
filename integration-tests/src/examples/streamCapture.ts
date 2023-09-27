import {
  testStreamCaptureSimple,
  testStreamCaptureReturn,
} from "../compiled/examples/streamCapture.js";

export async function streamCaptureSimpleCall() {
  return await testStreamCaptureSimple();
}

export async function streamCaptureReturnCall() {
  return await testStreamCaptureReturn();
}
