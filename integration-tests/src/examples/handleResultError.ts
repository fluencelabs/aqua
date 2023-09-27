import { handleResultError } from "../compiled/examples/handleResultError.js";

export async function handleResultErrorCall() {
  return await handleResultError(true, false);
}
