import { structuralTypingTest } from "../compiled/examples/structuraltyping.js";

export async function structuralTypingCall(): Promise<string> {
  return await structuralTypingTest();
}
